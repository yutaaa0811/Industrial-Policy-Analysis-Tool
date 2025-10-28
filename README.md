# 準備中

# このページは、「簡易的な産業政策評価ツール」のオンライン・アペンディクスです。

××の調査票情報（ミクロデータ）を用いて、産業政策の政策評価を簡易的に評価することができます。

本分析ツールは、３つのRコード（01_prepare.R、02_analysis.R、03_robustness）から構成されています。番号順に使用してください。

作成した分析ツールの詳細については、「簡易的な産業政策評価ツールの作成」をご覧ください。
[RIETI PDP（後日公表予定）]()
[html版](https://yutaaa0811.github.io/Industrial-Policy-Analysis-Tool/)

産業政策評価ツールを使用する際は、[●●.zip]()をご自身の環境にDLして、解凍して使用してください。「data」フォルダ内に、下記利用者が個別に準備するファイルを格納して下さい。

# 使用するファイル

## [01_prepare.R](https://github.com/yutaaa0811/Industrial-Policy-Analysis-Tool/blob/main/01_prepare.R)

××のcsv形式の調査票情報をパネルデータ化することができます。デフォルトでは、1995年から2023年のデータが必要です。

パネルデータは、merged_key.csvというファイル名で出力されます。

## [02_analysis.R](https://github.com/yutaaa0811/Industrial-Policy-Analysis-Tool/blob/main/02_analysis.R)

対象となる産業政策に関して差の差分析を行うことができます（本文の分析）。その際、「merged_key.csv」、「企業リスト.xlsx」、「industry_name.csv」を参照するため、参照先に格納されていることを確認してください。

## [03_robustness.R](https://github.com/yutaaa0811/Industrial-Policy-Analysis-Tool/blob/main/03_robustness.R)

分析結果の頑健性を確認する分析を行うことができます（Appendixの分析）。その際、「merged_key.csv」、「企業リスト.xlsx」、「industry_name.csv」を参照するため、参照先に格納されていることを確認してください。

## その他の提供ファイル

分析で用いる産業分類の一覧として、industry_name.csvを使用します。

サンプル用の分析データとして、●●の対象となった企業のリストを提供します（企業リスト.xlsx）。

## 利用者が個別に準備するファイル

「公的統計の二次的利用サービス」に関する[独立行政法人統計センター](https://www.nstac.go.jp/)のページを熟読の上、ご自身で申請して××の調査票情報を用意してください。調査票情報の扱いには十分に注意して下さい。

分析対象としたい政策の介入群の企業のリストをxlsx形式でご準備ください。
