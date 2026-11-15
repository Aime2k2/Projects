
import cv2
import numpy as np

# OpenCV uses BGR colors.
BG = (19, 22, 29)
PANEL = (31, 36, 46)
PANEL_LIGHT = (43, 49, 62)
WHITE = (245, 246, 248)
MUTED = (157, 166, 181)
GREEN = (90, 225, 115)
RED = (65, 80, 245)
ACCENT = (240, 185, 95)

WIDTH = 1280
HEIGHT = 800

HAND_CONNECTIONS = [
    (0, 1), (1, 2), (2, 3), (3, 4),
    (0, 5), (5, 6), (6, 7), (7, 8),
    (5, 9), (9, 10), (10, 11), (11, 12),
    (9, 13), (13, 14), (14, 15), (15, 16),
    (13, 17), (17, 18), (18, 19), (19, 20),
    (0, 17),
]


def label(canvas, text, x, y, size=0.6,
          color=WHITE, thickness=1):
    cv2.putText(
        canvas, str(text), (int(x), int(y)),
        cv2.FONT_HERSHEY_SIMPLEX, size,
        color, thickness, cv2.LINE_AA
    )


def panel(canvas, x1, y1, x2, y2, color=PANEL):
    cv2.rectangle(
        canvas, (x1, y1), (x2, y2), color, -1
    )


def draw_hand(frame, landmarks, bbox):
    height, width = frame.shape[:2]

    points = [
        (int(p.x * width), int(p.y * height))
        for p in landmarks
    ]

    # Green skeleton
    for start, end in HAND_CONNECTIONS:
        cv2.line(
            frame, points[start], points[end],
            GREEN, 2, cv2.LINE_AA
        )

    # Red landmark dots
    for point in points:
        cv2.circle(
            frame, point, 4, RED, -1, cv2.LINE_AA
        )

    # Green bounding box
    x1, y1, x2, y2 = bbox
    cv2.rectangle(
        frame, (x1, y1), (x2, y2),
        GREEN, 2
    )


def wrap_text(text, max_width, scale=0.7, thickness=1):
    """Wrap text to fit inside an OpenCV panel."""
    if not text:
        return [""]

    result = []

    for paragraph in str(text).split("\n"):
        if not paragraph:
            result.append("")
            continue

        line = ""

        for word in paragraph.split(" "):
            candidate = f"{line} {word}".strip()
            measured = cv2.getTextSize(
                candidate, cv2.FONT_HERSHEY_SIMPLEX,
                scale, thickness
            )[0][0]

            if measured <= max_width:
                line = candidate
            else:
                if line:
                    result.append(line)

                line = ""

                # Split long words when necessary
                for char in word:
                    candidate = line + char
                    measured = cv2.getTextSize(
                        candidate, cv2.FONT_HERSHEY_SIMPLEX,
                        scale, thickness
                    )[0][0]

                    if measured > max_width and line:
                        result.append(line)
                        line = char
                    else:
                        line = candidate

        result.append(line)

    return result


def draw_dashboard(
    frame,
    predicted_letter,
    confidence,
    stable_letter,
    composed_text,
    editing=False,
    status="Ready"
):
    canvas = np.full(
        (HEIGHT, WIDTH, 3),
        BG, dtype=np.uint8
    )

    # Header
    label(
        canvas, "SIGNFRAME AI",
        36, 53, 1.05, GREEN, 2
    )
    label(
        canvas,
        "REAL-TIME ASL FINGERSPELLING / COMMUNICATION",
        38, 82, 0.43, MUTED
    )

    # Active recognition indicator
    cv2.circle(
        canvas, (1050, 48), 6,
        GREEN, -1, cv2.LINE_AA
    )
    label(
        canvas, "AI RECOGNITION",
        1065, 53, 0.46, GREEN
    )

    # Camera panel
    panel(canvas, 30, 110, 840, 585)
    label(
        canvas, "LIVE CAMERA",
        52, 145, 0.57, WHITE, 2
    )

    camera = cv2.resize(
        frame, (770, 395),
        interpolation=cv2.INTER_AREA
    )
    canvas[163:558, 50:820] = camera

    # Recognition panel
    panel(canvas, 860, 110, 1250, 585)
    label(
        canvas, "RECOGNITION",
        886, 145, 0.65, WHITE, 2
    )

    label(
        canvas, "PREDICTED LETTER",
        886, 192, 0.47, MUTED
    )
    label(
        canvas, predicted_letter or "-",
        898, 305, 3.4,
        GREEN if predicted_letter else MUTED, 5
    )

    label(
        canvas, "MODEL CONFIDENCE",
        886, 353, 0.47, MUTED
    )

    pct = max(0.0, min(float(confidence), 1.0))
    label(
        canvas, f"{pct * 100:.1f}%",
        886, 387, 0.8, WHITE, 2
    )

    cv2.rectangle(
        canvas, (886, 407), (1217, 424),
        PANEL_LIGHT, -1
    )
    cv2.rectangle(
        canvas, (886, 407),
        (886 + int(331 * pct), 424),
        GREEN, -1
    )

    label(
        canvas, "STABLE PREDICTION",
        886, 473, 0.47, MUTED
    )
    label(
        canvas,
        stable_letter or "Waiting...",
        886, 516, 0.9,
        GREEN if stable_letter else MUTED, 2
    )

    # Message panel
    panel(canvas, 30, 605, 1250, 735)
    label(
        canvas, "YOUR MESSAGE",
        52, 636, 0.55, GREEN, 2
    )

    if editing:
        label(
            canvas, "EDIT MODE",
            1090, 636, 0.5, ACCENT, 2
        )

    message = (
        composed_text
        if composed_text
        else "Your message will appear here..."
    )

    lines = wrap_text(message, 1130, 0.73, 2)

    for index, line in enumerate(lines[-2:]):
        label(
            canvas, line,
            52, 678 + index * 33,
            0.73,
            WHITE if composed_text else MUTED, 2
        )

    # Footer controls
    if editing:
        controls = (
            "TYPE TO EDIT  |  BACKSPACE  |  "
            "ENTER: SAVE  |  ESC: CANCEL"
        )
    else:
        controls = (
            "ENTER: ADD  |  SPACE  |  BACKSPACE  |  "
            "E: EDIT  |  V: SPEAK  |  "
            "F: DISPLAY  |  C: CLEAR  |  Q: QUIT"
        )

    label(
        canvas, controls,
        34, 769, 0.46, MUTED
    )
    label(
        canvas, str(status)[:85],
        35, 792, 0.40, GREEN
    )

    return canvas


def draw_message_screen(text):
    """Large communication screen without webcam preview."""
    canvas = np.full(
        (HEIGHT, WIDTH, 3),
        BG, dtype=np.uint8
    )

    label(
        canvas, "SIGNFRAME AI",
        40, 60, 0.9, GREEN, 2
    )
    label(
        canvas, "COMMUNICATION DISPLAY",
        40, 98, 0.52, MUTED
    )

    panel(canvas, 35, 125, 1245, 685)

    message = (
        str(text).strip()
        or "No message composed yet."
    )

    scale = 1.5
    lines = wrap_text(message, 1090, scale, 2)

    # Adjust font size for longer messages
    while len(lines) > 7 and scale > 0.55:
        scale = round(scale - 0.1, 2)
        lines = wrap_text(message, 1090, scale, 2)

    # Show the most recent lines if message is very long
    lines = lines[-9:]

    spacing = max(34, int(scale * 54))
    total_height = (len(lines) - 1) * spacing
    start_y = max(175, 405 - total_height // 2)

    for index, line in enumerate(lines):
        width = cv2.getTextSize(
            line, cv2.FONT_HERSHEY_SIMPLEX,
            scale, 2
        )[0][0]

        x = max(55, (WIDTH - width) // 2)

        label(
            canvas, line,
            x, start_y + index * spacing,
            scale, WHITE, 2
        )

    label(
        canvas,
        "F / ESC: BACK  |  V: SPEAK  |  Q: QUIT",
        40, 758, 0.56, GREEN
    )

    return canvas
