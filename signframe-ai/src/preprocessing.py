
import cv2
import numpy as np
import torch


IMAGE_SIZE = 28


def get_hand_bbox(landmarks, frame_shape, padding=0.25):
    """
    Calculate a square bounding box around the detected hand.

    padding:
        Fraction of additional space around the hand.
        Default 0.25 means 25% additional width/height.
    """
    height, width = frame_shape[:2]

    xs = np.array(
        [point.x * width for point in landmarks],
        dtype=np.float32
    )
    ys = np.array(
        [point.y * height for point in landmarks],
        dtype=np.float32
    )

    min_x, max_x = float(xs.min()), float(xs.max())
    min_y, max_y = float(ys.min()), float(ys.max())

    center_x = (min_x + max_x) / 2.0
    center_y = (min_y + max_y) / 2.0

    hand_width = max_x - min_x
    hand_height = max_y - min_y

    box_size = max(hand_width, hand_height)
    box_size *= (1.0 + padding)
    box_size = max(int(round(box_size)), 80)

    x1 = int(round(center_x - box_size / 2))
    y1 = int(round(center_y - box_size / 2))

    x2 = x1 + box_size
    y2 = y1 + box_size

    # Do not clip here: preprocessing will pad the crop
    # when the hand is close to a frame boundary.
    return x1, y1, x2, y2


def preprocess_hand(frame, bbox):
    """
    Convert the hand region into the CNN's expected input:
        shape: [1, 1, 28, 28]
        dtype: float32
        values: [0, 1]
    """
    height, width = frame.shape[:2]
    x1, y1, x2, y2 = map(int, bbox)

    if x2 <= x1 or y2 <= y1:
        return None

    # Calculate how much padding is needed outside the frame.
    pad_left = max(0, -x1)
    pad_top = max(0, -y1)
    pad_right = max(0, x2 - width)
    pad_bottom = max(0, y2 - height)

    # Extend the frame only when necessary.
    if any((pad_left, pad_top, pad_right, pad_bottom)):
        padded_frame = cv2.copyMakeBorder(
            frame,
            pad_top,
            pad_bottom,
            pad_left,
            pad_right,
            borderType=cv2.BORDER_REPLICATE
        )
    else:
        padded_frame = frame

    # Shift crop coordinates into the padded image.
    x1 += pad_left
    x2 += pad_left
    y1 += pad_top
    y2 += pad_top

    crop = padded_frame[y1:y2, x1:x2]

    if crop.size == 0:
        return None

    gray = cv2.cvtColor(crop, cv2.COLOR_BGR2GRAY)

    resized = cv2.resize(
        gray,
        (IMAGE_SIZE, IMAGE_SIZE),
        interpolation=cv2.INTER_AREA
    )

    normalized = resized.astype(np.float32) / 255.0

    tensor = torch.from_numpy(
        normalized
    ).unsqueeze(0).unsqueeze(0)

    return tensor
