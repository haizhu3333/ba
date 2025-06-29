import pandas as pd
from pathlib import Path
from sklearn.linear_model import LinearRegression

DATA_PATH = Path(__file__).parent.parent.joinpath('output', 'deals.csv')
TEST_PATH = Path(__file__).parent.parent.joinpath('output', 'test-deals.csv')

def load_data(csv_path: Path) -> tuple[pd.DataFrame, pd.Series]:
    df = pd.read_csv(
        csv_path, header=None, names=['S', 'H', 'D', 'C', 'tricks'], keep_default_na=False)
    cards = pd.DataFrame({
        f'{suit}{rank}': df[suit].str.contains(rank).astype(int)
        for suit in 'SHDC' for rank in 'AKQJT98765432'
    })
    tricks = df['tricks']
    return cards, tricks

def compute_hcp(df: pd.DataFrame) -> pd.DataFrame:
    return pd.DataFrame({
        'hcp': sum(value * df[f'{suit}{rank}']
                   for suit in 'SHDC'
                   for rank, value in zip('AKQJ', [4, 3, 2, 1]))
    })

if __name__ == '__main__':
    cards, tricks = load_data(DATA_PATH)
    hcp = compute_hcp(cards)
    model = LinearRegression()
    model.fit(hcp, tricks)
    print(f'{model.coef_=}, {model.intercept_=}')

    cards, tricks = load_data(TEST_PATH)
    hcp = compute_hcp(cards)
    preds = model.predict(hcp)
    gt = tricks.values
    print(preds.shape)
