FROM mysql:8.0
# バージョン8.0は照会順序などのデフォルト設定が5.7とは異なる
# https://mita2db.hateblo.jp/entry/2020/12/07/000000

# 文字コード/照会順序の設定
COPY ./config/db.cnf /etc/mysql/conf.d/db.cnf


# データベースとテーブルの作成
# COPY ./init/01_create_db.sql /docker-entrypoint-initdb.d/01_create_db.sql
# COPY ./init/02_create_data.sql /docker-entrypoint-initdb.d/02_create_data.sql

# twitter用
COPY ./init/03_twitter_create_db.sql /docker-entrypoint-initdb.d/03_twitter_create_db.sql
COPY ./init/04_twitter_create_data.sql /docker-entrypoint-initdb.d/04_twitter_create_data.sql


# 【失敗】永続化するためにローカルディレクトリをマウントしようとしたが、この方法だとdockerが管理する領域にデータが保存され、ユーザーがアクセスできないので、docker-composeからマウント先を指定する
# VOLUME ./data:/var/lib/mysql