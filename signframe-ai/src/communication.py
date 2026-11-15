
import platform
import shutil
import subprocess


class SpeechEngine:
    def __init__(self):
        self.process = None

    def speak(self, text):
        message = text.strip()

        if not message:
            return False, "Compose a message before speaking."

        self.stop()

        system = platform.system()

        try:
            if system == "Darwin":
                command = ["say", message]

            elif system == "Windows":
                # Windows built-in speech synthesizer.
                safe = message.replace("'", "''")
                script = (
                    "Add-Type -AssemblyName System.Speech; "
                    "$speaker = New-Object "
                    "System.Speech.Synthesis.SpeechSynthesizer; "
                    f"$speaker.Speak('{safe}')"
                )
                command = [
                    "powershell", "-NoProfile", "-Command", script
                ]

            elif shutil.which("spd-say"):
                command = ["spd-say", "-w", message]

            elif shutil.which("espeak"):
                command = ["espeak", message]

            else:
                return False, "No offline speech engine available."

            self.process = subprocess.Popen(
                command,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL
            )
            return True, "Speaking message..."

        except (OSError, ValueError) as error:
            return False, f"Speech unavailable: {error}"

    def stop(self):
        if self.process is not None:
            if self.process.poll() is None:
                self.process.terminate()
            self.process = None

    def close(self):
        self.stop()
