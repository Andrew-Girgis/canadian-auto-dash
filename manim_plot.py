import pandas as pd
from manim import *

# Load and normalize real data
df = pd.read_csv("manim_auto_exports.csv", parse_dates=["ref_date"])

# Show December 2024 (month after tariffs)
target_month = pd.to_datetime("2024-12-01")
data = df[df["ref_date"] == target_month].to_dict("records")

class StarScatterplot(Scene):
    def construct(self):
        axes = Axes(
            x_range=[0, 8, 1],
            y_range=[-3, 3, 1],
            x_length=7,
            y_length=4,
            axis_config={"include_tip": True}
        ).add_coordinates()

        # Axis labels
        x_label = Text("Total Exports (Billions CAD)", font_size=16)
        y_label = Text("% Change", font_size=16)

        x_label.next_to(axes.x_axis, DOWN, buff=0.4)
        y_label.next_to(axes.y_axis, LEFT, buff=0.4).rotate(PI / 2)

        self.play(Create(axes), Write(x_label), Write(y_label))

        animations = []
        flip = True  # Toggle for alternating label placement

        for point in data:
            x = point["x"]
            y = point["y"]
            label = point["geo"]
            raw_scale = point.get("scale", 0.1)
            scale = min(max(raw_scale, 0.02), 0.15)

            # Color based on direction
            if point["pct_change"] > 0:
                color = BLUE
            elif point["pct_change"] < 0:
                color = RED
            else:
                color = GRAY

            # Create star and label
            star = RegularPolygon(n=5, start_angle=PI / 2, color=color).scale(scale)
            star.move_to(axes.c2p(x, y))

            text = Text(label, font_size=12)
            direction = RIGHT if flip else LEFT
            text.next_to(star, direction, buff=0.15)
            flip = not flip  # alternate side

            animations += [FadeIn(star), Write(text)]

        self.play(*animations, run_time=4)
        self.wait()
