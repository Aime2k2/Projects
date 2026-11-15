"""Download missing SignFrame AI runtime assets using HTTPS."""
from pathlib import Path
from urllib.request import urlopen

ROOT = Path(__file__).resolve().parents[1]
FILES = {
    "assets/hand_landmarker.task":
        "https://storage.googleapis.com/mediapipe-models/"
        "hand_landmarker/hand_landmarker/float16/1/hand_landmarker.task",
    "models/feedback_candidate.pth":
        "https://raw.githubusercontent.com/tayyabastack/"
        "signframe-ai/main/models/feedback_candidate.pth",
}

def download(relative_path, url):
    dest = ROOT / relative_path
    if dest.is_file() and dest.stat().st_size > 100_000:
        print("[OK] Already present:", relative_path)
        return
    dest.parent.mkdir(parents=True, exist_ok=True)
    temp = dest.with_name(dest.name + ".download")
    print("[GET]", relative_path)
    try:
        with urlopen(url, timeout=90) as source, temp.open("wb") as target:
            while True:
                chunk = source.read(262144)
                if not chunk:
                    break
                target.write(chunk)
        if temp.stat().st_size < 100_000:
            raise RuntimeError("File unexpectedly small: " + relative_path)
        temp.replace(dest)
    except Exception:
        temp.unlink(missing_ok=True)
        raise
    print("[OK] Downloaded:", relative_path)

if __name__ == "__main__":
    for path, link in FILES.items():
        download(path, link)
    print("Run: python -m src.webcam_app")
