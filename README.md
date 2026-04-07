# 準備中

# 「産業政策分析ツール」オンライン・アペンディクス

本ページでは、差の差の手法を用いて産業政策の効果を探索的に分析する「産業政策評価ツール」を提供しています。ツールの詳細は、RIETI PDPをご確認ください。

「産業政策評価ツール」の使用には、Rの実行環境が必要です。あらかじめご準備ください。

「Industrial-Policy-Analysis-Tool.zip」ファイルをダウンロードして、作業環境下で解凍してください。分析用コード、データ格納フォルダ、出力フォルダ、が格納されています。本分析ツールは、３つのRコード（01_prepare.R、02_analysis.R、03_robustness）から構成されています。番号順に使用してください。

<p align="center">
<img width="400" height="560" alt="image" src="https://github.com/user-attachments/assets/cf2fbc55-9bf8-4572-a2f2-19895372e831" />
</p>

作成した分析ツールの詳細については、「簡易的な産業政策評価ツールの作成」をご覧ください。
[RIETI PDP（後日公表予定）]()
<!-- [html版](https://yutaaa0811.github.io/Industrial-Policy-Analysis-Tool/) -->

# 必要データの準備

## 利用者が個別に準備するファイル

kikatsu_rawdataのフォルダには、経済産業省企業活動基本調査の調査票情報（個票データ）を格納してください。

「公的統計の二次的利用サービス」に関する[独立行政法人統計センター](https://www.nstac.go.jp/)のページを熟読の上、ご自身で申請して経済産業省企業活動基本調査の調査票情報を用意してください。

<p align="center">
<img width="400" height="335" alt="image" src="https://github.com/user-attachments/assets/165c7e04-0c26-4d9c-87e3-ecd721a1b3ce" />
</p>

treated_listには、処置群企業（政策介入を受けた企業）のリストを格納してください。分析対象としたい政策の介入群の企業のリストをxlsx形式でご準備ください。

<p align="center">
<img width="400" height="338" alt="image" src="https://github.com/user-attachments/assets/f630d017-61d1-44c6-91dd-b499defb00fc" />
</p>

## その他の提供ファイル

dataフォルダには、地図データと産業名リストが格納されています（利用者の作業不要）。

分析で用いる産業分類の一覧として、industry_name.csvを使用します。

<p align="center">
<img width="400" height="255" alt="image" src="https://github.com/user-attachments/assets/be249e47-0e97-4960-bae4-f8726c8bd922" />
</p>

# ツールの実行

## ①準備
### [01_prepare.R](https://github.com/yutaaa0811/Industrial-Policy-Analysis-Tool/blob/main/01_prepare.R)

企業活動基本調査の調査票情報（csv形式）を分析に適した形に整形します。設定セクションでワーキングディレクトリを設定してください。

コードを実行すると、「01_prepare_output」フォルダ内にmerged_key.csv等のファイルが出力されます。正しく出力されているか確認して、次のステップに進んでください。

<p align="center">
<img width="400" height="595" alt="image" src="https://github.com/user-attachments/assets/fe1c9631-fc31-42e3-b85c-bb1a2aac61ac" />
</p>

## ②メイン分析
### [02_analysis.R](https://github.com/yutaaa0811/Industrial-Policy-Analysis-Tool/blob/main/02_analysis.R)

対象となる産業政策に関して差の差分析を行うことができます（本分析）。

02_analysis.Rを使って、メイン分析を実施します。ワーキングディレクトリを設定してください。また、分析対象の政策名を記入してください（出力ファイル名等に反映されます）。その際、「merged_key.csv」、「企業リスト.xlsx」、「industry_name.csv」を参照するため、参照先に格納されていることを確認してください。

コードを実行すると、 「02_analysis_output」フォルダ内に分析結果が格納されます。正しく出力されているか確認してください。

<p align="center">
<img width="400" height="343" alt="image" src="https://github.com/user-attachments/assets/aca8242d-7626-4bd6-9bb4-8cdc8d786dbe" />
</p>

### 分析結果

00から始まるファイルは記述統計量をまとめたものです。industyは産業別企業数の分布、map_ratioは都道府県別企業割合の分布、descriptiveは各変数ごとの記述統計量、等です。　
<p align="center">
<img width="400" height="100" alt="image" src="https://github.com/user-attachments/assets/b2f4d6d9-f64c-493e-97b3-5b7478ddc800" />
</p>

01~07のファイルは、メインの分析結果です。

<p align="center">
<img width="400" height="348" alt="image" src="https://github.com/user-attachments/assets/4e7e96e3-8376-4aa1-9fcc-0812faea8761" />
</p>

pdfとpngで図を、xlsxで数値を確認することができます。同じタイトルのファイルは、同じ分析結果を異なるファイル形式で保存したものです。

<p align="center">
<img width="400" height="206" alt="image" src="https://github.com/user-attachments/assets/d0f8f85b-ef8e-410d-ac29-cd4ee3bff2a3" />
</p>

## ③補足的分析
### [03_robustness.R](https://github.com/yutaaa0811/Industrial-Policy-Analysis-Tool/blob/main/03_robustness.R)

分析結果の頑健性を確認する分析を行うことができます（Appendixの分析）。ワーキングディレクトリを設定してください。また、分析対象の政策名を記入してください（出力ファイル名等に反映されます）。その際、「merged_key.csv」、「企業リスト.xlsx」、「industry_name.csv」を参照するため、参照先に格納されていることを確認してください。

コードを実行すると、 「03_robustness_output」フォルダ内に分析結果が格納されます。正しく出力されているか確認してください。

<p align="center">
<img width="400" height="344" alt="image" src="https://github.com/user-attachments/assets/fd6e6df0-ba31-41cc-be0e-3c363280b29e" />
</p>

### 分析結果

2〇から始まるファイルは、通常の固定効果モデル（TWFE）を用いた結果です。

<p align="center">
<img width="400" height="233" alt="image" src="https://github.com/user-attachments/assets/9f7e20c6-0bf2-4b0b-ad5a-ca22734f6cd1" />
</p>

3〇から始まるファイルは、分析対象年を絞った分析の結果です。　

<p align="center">
<img width="400" height="251" alt="image" src="https://github.com/user-attachments/assets/74939889-86d5-4896-a933-77e419800073" />
</p>

4〇から始まるファイルは、処置タイミングによる処置効果の異質性に関する分析結果です。

<p align="center">
<img width="400" height="168" alt="image" src="https://github.com/user-attachments/assets/cfe9da49-8608-4b2c-8358-2b9042a046c2" />
</p>

5〇から始まるファイルは、マッチング手法に関する頑健性チェックの結果です。

<p align="center">
<img width="400" height="436" alt="image" src="https://github.com/user-attachments/assets/c484e9ab-7c1b-4c47-9531-fd74210fae92" />
</p>

# 引用

本分析ツールを利用する際は、以下を引用してください。

×××

# 免責事項

本分析ツールを利用して得られた分析結果に対して、作成者は一切の責任を負いません。
