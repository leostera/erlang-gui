#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate rustler;

use rustler::{Env, Error, Term};

use std::fs::File;
use std::io::Write;

use skia_safe::{Color, Data, EncodedImageFormat, Paint, PaintStyle, Path, Surface};
use std::mem;

pub struct Canvas {
    surface: Surface,
    path: Path,
    paint: Paint,
}

impl Canvas {
    pub fn new(width: i32, height: i32) -> Canvas {
        let mut surface = Surface::new_raster_n32_premul((width, height)).expect("no surface!");
        let path = Path::new();
        let mut paint = Paint::default();
        paint.set_color(Color::BLACK);
        paint.set_anti_alias(true);
        paint.set_stroke_width(1.0);
        surface.canvas().clear(Color::WHITE);
        Canvas {
            surface,
            path,
            paint,
        }
    }

    #[inline]
    pub fn save(&mut self) {
        self.canvas().save();
    }

    #[inline]
    pub fn translate(&mut self, dx: f32, dy: f32) {
        self.canvas().translate((dx, dy));
    }

    #[inline]
    pub fn scale(&mut self, sx: f32, sy: f32) {
        self.canvas().scale((sx, sy));
    }

    #[inline]
    pub fn move_to(&mut self, x: f32, y: f32) {
        self.begin_path();
        self.path.move_to((x, y));
    }

    #[inline]
    pub fn line_to(&mut self, x: f32, y: f32) {
        self.path.line_to((x, y));
    }

    #[inline]
    pub fn quad_to(&mut self, cpx: f32, cpy: f32, x: f32, y: f32) {
        self.path.quad_to((cpx, cpy), (x, y));
    }

    #[allow(dead_code)]
    #[inline]
    pub fn bezier_curve_to(&mut self, cp1x: f32, cp1y: f32, cp2x: f32, cp2y: f32, x: f32, y: f32) {
        self.path.cubic_to((cp1x, cp1y), (cp2x, cp2y), (x, y));
    }

    #[allow(dead_code)]
    #[inline]
    pub fn close_path(&mut self) {
        self.path.close();
    }

    #[inline]
    pub fn begin_path(&mut self) {
        let new_path = Path::new();
        self.surface.canvas().draw_path(&self.path, &self.paint);
        mem::replace(&mut self.path, new_path);
    }

    #[inline]
    pub fn stroke(&mut self) {
        self.paint.set_style(PaintStyle::Stroke);
        self.surface.canvas().draw_path(&self.path, &self.paint);
    }

    #[inline]
    pub fn fill(&mut self) {
        self.paint.set_style(PaintStyle::Fill);
        self.surface.canvas().draw_path(&self.path, &self.paint);
    }

    #[inline]
    pub fn set_line_width(&mut self, width: f32) {
        self.paint.set_stroke_width(width);
    }

    #[inline]
    pub fn data(&mut self) -> Data {
        let image = self.surface.image_snapshot();
        image.encode_to_data(EncodedImageFormat::PNG).unwrap()
    }

    #[inline]
    fn canvas(&mut self) -> &mut skia_safe::Canvas {
        self.surface.canvas()
    }
}

mod atoms {
    rustler_atoms! {
        atom ok;
        atom error;
        //atom __true__ = "true";
        //atom __false__ = "false";
    }
}

rustler::rustler_export_nifs! {
    "Elixir.Skia.Native",
    [
        ("draw", 0, draw),
    ],
    None
}

fn draw<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<(), Error> {
    let mut canvas = Canvas::new(2560, 1280);
    canvas.scale(1.2, 1.2);
    canvas.move_to(36.0, 48.0);
    canvas.quad_to(660.0, 880.0, 1200.0, 360.0);
    canvas.translate(10.0, 10.0);
    canvas.set_line_width(20.0);
    canvas.stroke();
    canvas.save();
    canvas.move_to(30.0, 90.0);
    canvas.line_to(110.0, 20.0);
    canvas.line_to(240.0, 130.0);
    canvas.line_to(60.0, 130.0);
    canvas.line_to(190.0, 20.0);
    canvas.line_to(270.0, 90.0);
    canvas.fill();
    let d = canvas.data();
    let mut file = File::create("test.png").unwrap();
    let bytes = d.as_bytes();
    file.write_all(bytes).unwrap();
    Ok(())
}

fn noop<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<(), Error> {
    Ok(())
}
