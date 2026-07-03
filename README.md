# DEMからTWI/TPIを計算する

DEM (数値標高モデル) から地形湿潤指数 (Topographic Wetness Index, TWI) や地形位置指数 (Topographic Position Index, TPI) を R で計算する方法をまとめたリポジトリです。

公開サイトはこちらです。

- <https://maple60.github.io/dem-twi-r/>

## 内容

このリポジトリには、DEM データの準備から TWI/TPI の計算までを確認できる Quarto ノートブックと、TWI の概念デモを含めています。

- [DEMデータの準備](notebooks/dem_download.qmd)
- [地形湿潤指数 (TWI) の計算](notebooks/topographic_wetness_index.qmd)
- [地形位置指数 (TPI) の計算](notebooks/topographic_position_index.qmd)
- [Shinylive による TWI の概念デモ](app/)

Shinylive 版は静的サイト上で動く概念デモです。WhiteboxTools を Shinylive 上で動かすことは難しいため、自分の DEM やサンプルデータを使った計算には、別途公開している TWI/TPI 計算アプリを利用してください。

- [TWI/TPI計算アプリ](https://maple60-dem-terrain-indices-app.share.connect.posit.cloud/)
- [アプリのソースコード](https://github.com/maple60/dem-terrain-indices-app)

## ローカルでの実行

このリポジトリでは R パッケージ管理に [renv](https://rstudio.github.io/renv/) を使用しています。クローン後、R コンソールで以下を実行すると必要なパッケージを復元できます。

```r
# install.packages("renv") # まだインストールしていない場合
renv::restore()
```

Quarto が利用できる環境では、次のコマンドでサイトをプレビューできます。

```sh
quarto preview
```

## リポジトリ構成

- `index.qmd`: サイトのトップページ
- `notebooks/`: DEM、TWI、TPI の計算ノートブック
- `app/`: Shinylive で公開する概念デモ
- `app-shinylive/`: Shinylive 変換前のアプリソース
- `scripts/`: Shinylive 関連の補助スクリプト

## ライセンス

このリポジトリは MIT License の下で公開しています。詳細は [LICENSE](LICENSE) を参照してください。
