
from collections import Counter, deque


class PredictionStabilizer:
    def __init__(
        self,
        window_size=12,
        required_votes=9,
        confidence_threshold=0.65
    ):
        self.history = deque(maxlen=window_size)
        self.required_votes = required_votes
        self.confidence_threshold = confidence_threshold

    def update(self, letter, confidence):
        if confidence < self.confidence_threshold:
            self.history.append(None)
        else:
            self.history.append(letter)

        counts = Counter(
            item for item in self.history
            if item is not None
        )

        if not counts:
            return None

        best_letter, votes = counts.most_common(1)[0]

        if votes >= self.required_votes:
            return best_letter

        return None

    def reset(self):
        self.history.clear()
