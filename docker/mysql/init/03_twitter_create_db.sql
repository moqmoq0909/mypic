CREATE DATABASE IF NOT EXISTS tags;
USE tags;

-- creator,book,media: 漫画・動画,twitter,pixiv
-- book media: pintarest(特定のジャンルで検索),th,kリブ(制作者ごとのまとめは無く、作品だけが存在するジャンル)
-- media: その他

-- CREATE TABLE IF NOT EXISTS images (
--   id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
--   image_key      VARCHAR(255)   NOT NULL, --twitterのidなど
--   image_name    VARCHAR(255), -- 作者名,アカウント,作品のタイトル
--   image_text    VARCHAR(255), -- プロフィール、ツイート内容、詳細、メモ
--   image_url    VARCHAR(255)   NOT NULL,
--   image_type      VARCHAR(10)   NOT NULL, --image,video
--   relation_type      VARCHAR(10)   NOT NULL, -- creator,book,image
--   media_type      VARCHAR(10)   NOT NULL, -- twitter, pixiv
--   has_read          BOOLEAN   NOT NULL,
--   parent_id    INT   NOT NULL, -- 親のidが格納される
--   uploaded_at DATETIME NOT NULL,
-- ) ENGINE = InnoDB;

CREATE TABLE IF NOT EXISTS creators (
  id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  creator_key      VARCHAR(255)   NOT NULL,
  creator_name    VARCHAR(255)   NOT NULL,
  profile           JSON
) ENGINE = InnoDB;

CREATE TABLE IF NOT EXISTS books (
  id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  book_key      VARCHAR(255)   NOT NULL,
  book_name    VARCHAR(255),
  book_text    VARCHAR(255),
  has_read          BOOLEAN   NOT NULL,
  media_type      VARCHAR(10)   NOT NULL,
  images            JSON      NOT NULL,
  creator_id    INT   NOT NULL,
  -- uploaded_at DATETIME NOT NULL,
  created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  FOREIGN KEY(creator_id) REFERENCES creators(id)
) ENGINE = InnoDB;



-- creatorはcomit,twitter,pinterestなどのパーテーションを設定しなくてもよい(ユーザー名はサービスをまたいでいる可能性が高いので)、一方bookはパーテーションを設定する
-- bookを基本単位として、imagesカラムの中にimage_id,image_urlなどのkeyを持つjsonを格納する
-- imageしかない場合もbookとして登録し、imagesには一つの画像のみ登録する
-- 作者のわからないimageやbookはunknownというcreatorに紐づける
-- books_tags中間テーブルには、book_id,tag_idとimage_idを保存する。imageへのタグ付けが無ければimage_tagは偽値を設定する
-- (2,4,4,0) これはbookへのタグ付け
-- (2,4,4,5) これはimageへのタグ付け

-- テーブルはcreator,bookまでにしてimageはbookのjson型で表現
-- creatorのパーテーションはなし、bookのパーテーションはsns_type
-- ★MySQLでパーティション化されたテーブルに外部キー制約を設定することはできないらしいので、パーテーションは諦める

-- ※もし、パーテーションを二重にする場合は、パーテーション×パーテーションの総数で一重にする。sns_type×何軍

-- comic_1(軍)
-- comic_2
-- comic_3

-- twitter_1
-- twitter_2
-- twitter_3

CREATE TABLE IF NOT EXISTS categories (
  id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  category_key      VARCHAR(255)   NOT NULL,
  category_name    VARCHAR(255)   NOT NULL,
  category_color    VARCHAR(255)   NOT NULL
) ENGINE = InnoDB;

CREATE TABLE IF NOT EXISTS tags (
  id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  tag_key      VARCHAR(255)   NOT NULL,
  tag_name    VARCHAR(255)   NOT NULL,
  category_id      INT NOT NULL,
  FOREIGN KEY(category_id) REFERENCES categories(id)
) ENGINE = InnoDB;

CREATE TABLE IF NOT EXISTS books_tags(
  id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  tag_id INT NOT NULL,
  book_id INT NOT NULL,
  image_id INT NOT NULL
) ENGINE = InnoDB;