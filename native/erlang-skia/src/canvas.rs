use skia_safe::{Color, Paint, PaintStyle, Path, PictureRecorder, Rect};
use std::mem;

pub struct Canvas {
    recorder: PictureRecorder,
    path: Path,
    paint: Paint,
}

impl Canvas {
    pub fn new(width: i32, height: i32) -> Canvas {
        let mut recorder = PictureRecorder::new();
        let bounds = Rect::from_iwh(width, height);
        let canvas = recorder.begin_recording(bounds, None, None);
        let path = Path::new();
        let mut paint = Paint::default();
        paint.set_color(Color::BLACK);
        paint.set_anti_alias(true);
        paint.set_stroke_width(1.0);
        canvas.clear(Color::WHITE);
        Canvas {
            recorder,
            path,
            paint,
        }
    }

    #[inline]
    pub fn as_bytes(&mut self) -> Vec<u8> {
        match self.recorder.finish_recording_as_picture(None) {
            Some(picture) => picture.serialize().as_bytes().to_vec(),
            _ => vec![],
        }
    }

    #[inline]
    pub fn save(&mut self) {
        self.recorder.recording_canvas().save();
    }

    #[inline]
    pub fn translate(&mut self, dx: f32, dy: f32) {
        self.recorder.recording_canvas().translate((dx, dy));
    }

    #[inline]
    pub fn scale(&mut self, sx: f32, sy: f32) {
        self.recorder.recording_canvas().scale((sx, sy));
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
        self.recorder
            .recording_canvas()
            .draw_path(&self.path, &self.paint);
        mem::replace(&mut self.path, new_path);
    }

    #[inline]
    pub fn stroke(&mut self) {
        self.paint.set_style(PaintStyle::Stroke);
        self.recorder
            .recording_canvas()
            .draw_path(&self.path, &self.paint);
    }

    #[inline]
    pub fn fill(&mut self) {
        self.paint.set_style(PaintStyle::Fill);
        self.recorder
            .recording_canvas()
            .draw_path(&self.path, &self.paint);
    }

    #[inline]
    pub fn set_line_width(&mut self, width: f32) {
        self.paint.set_stroke_width(width);
    }
}
