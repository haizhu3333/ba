import pandas as pd
from sklearn.linear_model import LinearRegression
import sys

def load_data(csv_path: str) -> pd.DataFrame:
    return pd.read_csv(
        csv_path, header=None, names=['S', 'H', 'D', 'C', 'tricks'], keep_default_na=False)

def expand(df: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
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
    data = load_data(sys.argv[1])
    cards, tricks = expand(data)
    hcp = compute_hcp(cards)
    model = LinearRegression()
    model.fit(hcp, tricks)
    print(f'{model.coef_=}, {model.intercept_=}')
