
import cv2

from src.hand_tracker import HandTracker
from src.preprocessing import get_hand_bbox, preprocess_hand
from src.inference import SignPredictor
from src.stabilizer import PredictionStabilizer
from src.text_composer import TextComposer
from src.visualizer import (
    draw_hand,
    draw_dashboard,
    draw_message_screen,
    WIDTH,
    HEIGHT,
)
from src.communication import SpeechEngine


WINDOW_NAME = "SignFrame AI"


def main():
    print("\n" + "=" * 55)
    print("SIGNFRAME AI - ASL RECOGNITION & COMMUNICATION")
    print("=" * 55)

    # Initialize application components
    tracker = HandTracker()
    predictor = SignPredictor()
    stabilizer = PredictionStabilizer()
    composer = TextComposer()
    speech = SpeechEngine()

    # Initialize webcam
    camera = cv2.VideoCapture(0)

    if not camera.isOpened():
        tracker.close()
        speech.close()
        raise RuntimeError("Unable to open webcam.")

    camera.set(cv2.CAP_PROP_FRAME_WIDTH, 1280)
    camera.set(cv2.CAP_PROP_FRAME_HEIGHT, 720)

    cv2.namedWindow(WINDOW_NAME, cv2.WINDOW_NORMAL)
    cv2.resizeWindow(WINDOW_NAME, WIDTH, HEIGHT)

    editing = False
    edit_buffer = ""
    display_mode = False

    status = "Ready - Show an ASL hand sign"

    print("\nKEYBOARD CONTROLS")
    print("-" * 55)
    print("ENTER       Add stable predicted letter")
    print("SPACE       Insert space")
    print("BACKSPACE   Delete last character")
    print("E           Edit message")
    print("V           Speak message")
    print("F           Full-screen message display")
    print("C           Clear message")
    print("Q / ESC     Quit")
    print("-" * 55)

    try:
        while True:
            predicted_letter = None
            confidence = 0.0
            stable_letter = None

            # ---------------------------------------
            # Full-screen communication display
            # ---------------------------------------
            if display_mode:
                screen = draw_message_screen(composer.text)

            else:
                success, frame = camera.read()

                if not success:
                    status = "Unable to read webcam frame."
                    print(status)
                    break

                # Mirror webcam image
                frame = cv2.flip(frame, 1)
                clean_frame = frame.copy()

                # ---------------------------------------
                # Hand detection and CNN prediction
                # ---------------------------------------
                if not editing:
                    landmarks = tracker.detect(clean_frame)

                    if landmarks:
                        bbox = get_hand_bbox(
                            landmarks,
                            clean_frame.shape
                        )

                        image_tensor = preprocess_hand(
                            clean_frame,
                            bbox
                        )

                        if image_tensor is not None:
                            predicted_letter, confidence = (
                                predictor.predict(image_tensor)
                            )

                            stable_letter = stabilizer.update(
                                predicted_letter,
                                confidence
                            )

                        else:
                            stabilizer.reset()

                        # Draw hand landmarks and bounding box
                        draw_hand(frame, landmarks, bbox)

                    else:
                        stabilizer.reset()

                else:
                    stabilizer.reset()

                # ---------------------------------------
                # Compose dashboard
                # ---------------------------------------
                visible_text = (
                    edit_buffer
                    if editing
                    else composer.text
                )

                screen = draw_dashboard(
                    frame=frame,
                    predicted_letter=predicted_letter,
                    confidence=confidence,
                    stable_letter=stable_letter,
                    composed_text=visible_text,
                    editing=editing,
                    status=status,
                )

            cv2.imshow(WINDOW_NAME, screen)

            key = cv2.waitKeyEx(1)

            if key == -1:
                continue

            # ---------------------------------------
            # Communication display controls
            # ---------------------------------------
            if display_mode:
                if key in (ord("f"), ord("F"), 27):
                    display_mode = False
                    status = "Returned to dashboard."

                elif key in (ord("v"), ord("V")):
                    _, status = speech.speak(
                        composer.text
                    )

                elif key in (ord("q"), ord("Q")):
                    break

                continue

            # ---------------------------------------
            # Message editing mode
            # ---------------------------------------
            if editing:
                if key in (10, 13):
                    composer.text = edit_buffer
                    editing = False
                    status = "Message saved."

                elif key == 27:
                    editing = False
                    status = "Editing cancelled."

                elif key in (8, 127):
                    edit_buffer = edit_buffer[:-1]

                elif key == 32:
                    edit_buffer += " "

                elif 33 <= key <= 126:
                    edit_buffer += chr(key)

                continue

            # ---------------------------------------
            # Normal keyboard controls
            # ---------------------------------------
            if key in (ord("q"), ord("Q"), 27):
                break

            elif key in (10, 13):
                if stable_letter:
                    composer.append_letter(
                        stable_letter
                    )

                    stabilizer.reset()

                    status = (
                        f"Added letter: {stable_letter}"
                    )

                else:
                    status = (
                        "Wait for a stable prediction."
                    )

            elif key == 32:
                composer.add_space()
                status = "Space inserted."

            elif key in (8, 127):
                composer.backspace()
                status = "Last character deleted."

            elif key in (ord("c"), ord("C")):
                composer.clear()
                stabilizer.reset()
                status = "Message cleared."

            elif key in (ord("e"), ord("E")):
                edit_buffer = composer.text
                editing = True
                stabilizer.reset()
                status = "Editing message."

            elif key in (ord("v"), ord("V")):
                _, status = speech.speak(
                    composer.text
                )

            elif key in (ord("f"), ord("F")):
                display_mode = True
                stabilizer.reset()
                status = "Communication display opened."

    finally:
        camera.release()
        tracker.close()
        speech.close()
        cv2.destroyAllWindows()

    print("\n" + "=" * 55)
    print("SIGNFRAME AI - SESSION ENDED")
    print("=" * 55)
    print("Final message:")
    print(composer.text)
    print("=" * 55)


if __name__ == "__main__":
    main()
