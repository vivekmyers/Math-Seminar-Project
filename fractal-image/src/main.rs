extern crate image;
extern crate num;
extern crate itertools;
extern crate scoped_threadpool;
extern crate time;
use num::Complex;
use num::Zero;
use itertools::Itertools;
use std::sync::{Arc, Mutex};
use std::str::FromStr;

#[derive(Clone, Copy)]
enum Algorithm {
    Newton, Halley, Laguerre, Secant,
}

impl FromStr for Algorithm {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Algorithm::*;
        Ok(match s {
            "n" => Newton,
            "h" => Halley,
            "l" => Laguerre,
            "s" => Secant,
            _ => Err(())?
        })
    }
}

#[derive(Clone, Copy)]
enum ShadingAlgorithm {
    Convergence, Result, Difference, Distance, Cycle,
}

impl FromStr for ShadingAlgorithm {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use ShadingAlgorithm::*;
        Ok(match s {
            "c" => Convergence,
            "i" => Result,
            "dc" => Difference,
            "d1" => Distance,
            "d2" => Cycle,
            _ => Err(())?
        })
    }
}

fn main() {
    let mut args = std::env::args().skip(1);
    let image_width: u32 = args.next().unwrap().parse().unwrap();
    let image_height: u32 = args.next().unwrap().parse().unwrap();

    let center_x: f64 = args.next().unwrap().parse().unwrap();
    let center_y: f64 = args.next().unwrap().parse().unwrap();
    let initial_radius: f64 = args.next().unwrap().parse().unwrap();

    let frames: usize = args.next().unwrap().parse().unwrap();
    let iterations: usize = args.next().unwrap().parse().unwrap();
    let tolerance: f64 = args.next().unwrap().parse().unwrap();
    let supersampling: usize = args.next().unwrap().parse().unwrap();

    let algorithm: Algorithm = args.next().unwrap().parse().unwrap();

    let (zp_start, zp_start_relative) = match algorithm {
        Algorithm::Secant => {
            let rel = args.next().unwrap().chars().next().unwrap() == 'r';
            let real: f64 = args.next().unwrap().parse().unwrap();
            let imag: f64 = args.next().unwrap().parse().unwrap();
            (Complex::new(real, imag), rel)
        },
        _ => (Complex::zero(), false)
    };

    let shading_algorithm: ShadingAlgorithm = args.next().unwrap().parse().unwrap();

    let remaining_args: Vec<String> = args.collect();
    let coefficients: Vec<Complex<f64>> = remaining_args.chunks(2).map(|s| Complex::new(s[0].parse().unwrap(),s[1].parse().unwrap())).collect();

    let num_threads = 64;
    let chunk_size = ((image_width * image_height - 1) / (num_threads - 1)) as usize;
    let mut pool = scoped_threadpool::Pool::new(num_threads);

    for i in 0..frames {
        println!("Beginning frame {}", i);
        let frame_start = time::PreciseTime::now();
        let radius = initial_radius / (0.1 * i as f64).exp2();
        let image_data = Arc::new(Mutex::new(vec![0u8; (image_width * image_height) as usize * 3]));

        let chunks = (0..image_width).cartesian_product(0..image_height).chunks(chunk_size);

        let mut threads_created = None;
        pool.scoped(|scope| {
            for c in &chunks {
                let points = c.collect::<Vec<_>>();
                let image_ref = Arc::clone(&image_data);
                let c = &coefficients;
                use ShadingAlgorithm::*;
                match shading_algorithm {
                    Convergence => scope.execute(move || {
                        for (x, y) in points.iter() {
                            let mut color = [0.0, 0.0, 0.0];
                            for si in 0..supersampling {
                                for sj in 0..supersampling {
                                    let mut sample_color = [0.0, 0.0, 0.0];
                                    let mut z = Complex::new(
                                        ((*x as f64 + si as f64 / supersampling as f64) / (image_width as f64) * 2.0 - 1.0) * radius + center_x,
                                        ((*y as f64 + sj as f64 / supersampling as f64) / (image_height as f64) * 2.0 - 1.0) * radius + center_y
                                    );
                                    let mut zp = zp_start + if zp_start_relative { z } else { Complex::zero() };
                                    for j in 0..iterations {
                                        use Algorithm::*;
                                        let dz = match algorithm {
                                            Newton => {
                                                eval(c, 0, z) / eval(c, 1, z)
                                            },
                                            Halley => {
                                                let f = eval(c, 0, z);
                                                let fp = eval(c, 1, z);
                                                f * fp / (fp * fp - 0.5 * f * eval(c, 2, z))
                                            },
                                            Laguerre => {
                                                let f = eval(c, 0, z);
                                                let g = eval(c, 1, z) / f;
                                                let h = eval(c, 2, z) / f;
                                                let deg = c.len() as f64 - 1.0;
                                                let q = ((deg - 1.0) * ((deg - 1.0) * (g * g - h) - h)).sqrt();
                                                let d1 = g + q;
                                                let d2 = g - q;
                                                Complex::new(deg, 0.0) / if d1.norm_sqr() > d2.norm_sqr() { d1 } else { d2 }
                                            },
                                            Secant => {
                                                let f = eval(c, 0, z);
                                                let fp = eval(c, 0, zp);
                                                (z - zp) * f / (f - fp)
                                            },
                                        };
                                        zp = z;
                                        z -= dz;
                                        if dz.norm_sqr() < tolerance {
                                            sample_color = shade(color_wheel_flat(z), j, 0.0, supersampling);
                                            break;
                                        }
                                    }
                                    for k in 0..3 {color[k] += sample_color[k]; }
                                }
                            }
                            let mut image = image_ref.lock().unwrap();
                            let pos = (y * image_width + x) as usize * 3;
                            for k in 0..3 { image[pos+k] = (255.9*color[k]) as u8; }
                        }
                    }),
                    Result => scope.execute(move || {
                        for (x, y) in points.iter() {
                            let mut color = [0.0, 0.0, 0.0];
                            for si in 0..supersampling {
                                for sj in 0..supersampling {
                                    let mut z = Complex::new(
                                        ((*x as f64 + si as f64 / supersampling as f64) / (image_width as f64) * 2.0 - 1.0) * radius + center_x,
                                        ((*y as f64 + sj as f64 / supersampling as f64) / (image_height as f64) * 2.0 - 1.0) * radius + center_y
                                    );
                                    let mut zp = zp_start + if zp_start_relative { z } else { Complex::zero() };
                                    for _ in 0..iterations {
                                        use Algorithm::*;
                                        let dz = match algorithm {
                                            Newton => {
                                                eval(c, 0, z) / eval(c, 1, z)
                                            },
                                            Halley => {
                                                let f = eval(c, 0, z);
                                                let fp = eval(c, 1, z);
                                                f * fp / (fp * fp - 0.5 * f * eval(c, 2, z))
                                            },
                                            Laguerre => {
                                                let f = eval(c, 0, z);
                                                let g = eval(c, 1, z) / f;
                                                let h = eval(c, 2, z) / f;
                                                let deg = c.len() as f64 - 1.0;
                                                let q = ((deg - 1.0) * ((deg - 1.0) * (g * g - h) - h)).sqrt();
                                                let d1 = g + q;
                                                let d2 = g - q;
                                                Complex::new(deg, 0.0) / if d1.norm_sqr() > d2.norm_sqr() { d1 } else { d2 }
                                            },
                                            Secant => {
                                                let f = eval(c, 0, z);
                                                let fp = eval(c, 0, zp);
                                                (z - zp) * f / (f - fp)
                                            },
                                        };
                                        zp = z;
                                        z -= dz;
                                        if dz.norm_sqr() < tolerance {
                                            break;
                                        }
                                    }
                                    let sample_color = shade_norm(color_wheel_flat(z), z.norm(), supersampling);
                                    for k in 0..3 {color[k] += sample_color[k]; }
                                }
                            }
                            let mut image = image_ref.lock().unwrap();
                            let pos = (y * image_width + x) as usize * 3;
                            for k in 0..3 { image[pos+k] = (255.9*color[k]) as u8; }
                        }
                    }),
                    _ => panic!()
                }
            }
            threads_created = Some(time::PreciseTime::now());
            let create_time = frame_start.to(threads_created.unwrap());
            println!("Created threads ({:.3}s)", create_time.num_milliseconds() as f64 * 0.001);
        });

        let data: Vec<u8> = Arc::try_unwrap(image_data).unwrap().into_inner().unwrap();
        let render_finished = time::PreciseTime::now();
        let render_time = frame_start.to(render_finished);
        println!("Rendering finished (total {:.3}s)", render_time.num_milliseconds() as f64 * 0.001);
        let image_buffer: image::ImageBuffer<image::Rgb<u8>, Vec<u8>> = image::ImageBuffer::from_vec(image_width, image_height, data).unwrap();
        let file_name = format!("images/img{:04}.png",i);
        image_buffer.save(&file_name).unwrap();
        let file_written = time::PreciseTime::now();
        let write_time = render_finished.to(file_written);
        println!("Wrote {} ({:.3}s)", file_name, write_time.num_milliseconds() as f64 * 0.001);
    }
}


fn eval(coefficients: &[Complex<f64>], diff_times: usize, z: Complex<f64>) -> Complex<f64> {
    let len = coefficients.len();
    let mut f = coefficients[0];
    for j in 0..diff_times {
        f *= (len - j - 1) as f64;
    }
    for i in 1..(len - diff_times) {
        f *= z;
        let mut a = coefficients[i];
        for j in 0..diff_times {
            a *= (len - i - j - 1) as f64;
        }
        f += a;
    }
    f
}

fn shade([r, g, b]: [f64; 3], iterations: usize, min: f64, ss: usize) -> [f64; 3] {
    let n = if iterations <= 3 { 2 } else { iterations - 2 };
    let scale = (min + (0.4-min)/(n as f64).powf(0.3) + 0.6*(-0.005*((n*n) as f64)).exp()) / (ss*ss) as f64;
    [r * scale, g * scale, b * scale]
}

fn shade_norm([r, g, b]: [f64; 3], norm: f64, ss: usize) -> [f64; 3] {
    let l = 1.0 - 1.0 / ((norm + 1.0).log(2.0) + 1.0);
    let c = 1.0 - (2.0 * l - 1.0).abs();
    let m = l - 0.5 * c;
    let scale = (ss*ss) as f64;
    [(r * c + m) / scale, (g * c + m) / scale, (b * c + m) / scale]
}

fn color_wheel_flat(z: Complex<f64>) -> [f64; 3] {
    let g = if z.norm_sqr() < 0.1 { 1.0 / (5e2*z.norm_sqr() + 1.0) } else { 0.0 };
    let h = -(-z.im).atan2(-z.re) * 3.0 / std::f64::consts::PI + 3.0;
    let x = 1.0 - (h % 2.0 - 1.0).abs();
    let mut result = if h < 1.0 {
        [1.0, x, 0.0]
    } else if h < 2.0 {
        [x, 1.0, 0.0]
    } else if h < 3.0 {
        [0.0, 1.0, x]
    } else if h < 4.0 {
        [0.0, x, 1.0]
    } else if h < 5.0 {
        [x, 0.0, 1.0]
    } else {
        [1.0, 0.0, x]
    };
    for i in 0..3 {
        result[i] *= 1.0 - g;
        result[i] += g;
    }
    result
}
