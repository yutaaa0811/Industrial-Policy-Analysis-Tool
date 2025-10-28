# 準備中

# このページは、「簡易的な産業政策評価ツール」のオンライン・アペンディクスです。

経済産業省企業活動基本調査の調査票情報を用いて、産業政策の政策評価を簡易的に評価することができます。ご自身で二次利用を申請して調査票情報を用意するとともに、分析対象としたい政策の介入群の企業のリストをxlsx形式でご準備ください。調査票情報の扱いには十分に注意して下さい。

本分析ツールは、３つのRコード（01_prepare.R、02_analysis.R、03_robustness）から構成されています。番号順に使用してください。

作成した分析ツールの詳細については、「簡易的な産業政策評価ツールの作成」をご覧ください。
[RIETI PDP（後日公表予定）]()
[html版](https://yutaaa0811.github.io/Industrial-Policy-Analysis-Tool-test/)

## [01_prepare.R](https://github.com/yutaaa0811/Industrial-Policy-Analysis-Tool-test/blob/main/01_prepare.R)

経済産業省企業活動基本調査のcsv形式の調査票情報をパネルデータ化することができます。merged_key.csvというファイル名で出力されます。

## [02_analysis.R](https://github.com/yutaaa0811/Industrial-Policy-Analysis-Tool-test/blob/main/02_analysis.R)

対象となる産業政策に関して差の差分析を行うことができます（本文の分析）。その際、「merged_key.csv」、「企業リスト.xlsx」、「industry_name.csv」を用いるようにしてください。

## [03_robustness.R](https://github.com/yutaaa0811/Industrial-Policy-Analysis-Tool-test/blob/main/03_robustness.R)

分析結果の頑健性を確認する分析を行うことができます（Appendixの分析）。その際、「merged_key.csv」、「企業リスト.xlsx」、「industry_name.csv」を用いるようにしてください。

