from typing import Callable
import numpy as np
import pandas as pd
from sklearn.linear_model import LinearRegression

import utils

def direct_linear_regression(train_cards: pd.DataFrame,
                             train_tricks: pd.Series,
                             test_cards: pd.DataFrame) -> np.ndarray:
    model = LinearRegression()
    model.fit(train_cards, train_tricks)
    print(f'Direct model\n{model.coef_=}\n{model.intercept_=}')
    return model.predict(test_cards)

def hcp_linear_regression(train_cards: pd.DataFrame,
                          train_tricks: pd.Series,
                          test_cards: pd.DataFrame) -> np.ndarray:
    hcp = utils.compute_hcp(train_cards)
    model = LinearRegression()
    model.fit(hcp, train_tricks)
    print(f'HCP model: {model.coef_=}, {model.intercept_=}')

    hcp = utils.compute_hcp(test_cards)
    return model.predict(hcp)


class ModelTester:

    def __init__(self):
        self.train_cards, self.train_tricks = utils.load_data(utils.DATA_PATH)
        self.test_cards, self.test_tricks = utils.load_data(utils.TEST_PATH)

    def test(self, model_fn: Callable[[pd.DataFrame, pd.Series, pd.DataFrame], np.ndarray]) -> float:
        preds = model_fn(self.train_cards, self.train_tricks, self.test_cards)
        ground_truth = self.test_tricks.values
        mse = np.mean((preds - ground_truth) ** 2)
        return mse.item()


if __name__ == '__main__':
    tester = ModelTester()
    print('Data loaded')

    direct_mse = tester.test(direct_linear_regression)
    print(f'Direct linear model: MSE = {direct_mse}')
    hcp_mse = tester.test(hcp_linear_regression)
    print(f'HCP linear model: MSE = {hcp_mse}')
