extern crate crossbeam;

use std::time::{SystemTime, UNIX_EPOCH};

use skia_safe::{paint::Style, Canvas, Color4f, Font, Paint, Rect};

pub struct FpsCounter {
    last_shown_at: u128,

    frame_count: u8,

    framerate: u8,

    text: String,
}

impl FpsCounter {
    pub fn new() -> FpsCounter {
        FpsCounter {
            last_shown_at: 0,
            frame_count: 0,
            framerate: 0,
            text: "".to_string(),
        }
    }

    pub fn tick(&mut self) -> () {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards")
            .as_millis();
        if (now - self.last_shown_at) > 1000 {
            self.text = format!("FPS: {:.1}", self.framerate);
            self.last_shown_at = now;
            self.framerate = self.frame_count;
            self.frame_count = 0;
        }
        self.frame_count += 1;
    }

    pub fn draw(&mut self, canvas: &mut Canvas) -> () {
        let rect = Rect::from_xywh(0.0, 0.0, 240.0, 80.0);

        let gray = Color4f::new(0.3, 0.3, 0.3, 1.0);
        let rect_paint = Paint::new(gray, None);

        canvas.draw_rect(rect, &rect_paint);

        let yellow = Color4f::new(1.0, 1.0, 0.0, 1.0);
        let mut text_paint = Paint::new(yellow, None);
        text_paint.set_anti_alias(true);
        text_paint.set_style(Style::StrokeAndFill);
        text_paint.set_stroke_width(1.0);

        let mut font = Font::default();
        font.set_size(40.0);

        canvas.draw_str(self.text.clone(), (50, 50), &font, &text_paint);
    }
}
