module Web.View.Stats.Index where
import Web.View.Prelude

data IndexView = IndexView {
    excuseCount :: Int,
    exerciseCount :: Int,
    currentExerciseStreak :: Int
}

instance View IndexView where
    html IndexView { .. }= [hsx|
        <p>Number of excuses: {excuseCount}</p>
        <p>Number of exercises: {exerciseCount}</p>
        <p>Current exercise streak: {currentExerciseStreak}</p>
    |]
