CREATE DATABASE IF NOT EXISTS mypic;
USE mypic;

-- groupsが予約語であったので、バッククオートで囲まないと文法エラーになる
-- CREATE TABLE IF NOT EXISTS `groups` (
--   group_id      INT            AUTO_INCREMENT PRIMARY KEY,
--   group_name    VARCHAR(255)   NOT NULL
-- ) ENGINE = InnoDB;

CREATE TABLE IF NOT EXISTS creator_groups (
  id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  creator_group_code      VARCHAR(255)   NOT NULL,
  creator_group_name    VARCHAR(255)   NOT NULL
) ENGINE = InnoDB;

CREATE TABLE IF NOT EXISTS creators (
  id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  creator_code      VARCHAR(255)   NOT NULL,
  creator_name    VARCHAR(255)   NOT NULL,
  creator_group_id    INT   NOT NULL,
  FOREIGN KEY(creator_group_id) REFERENCES creator_groups(id)
) ENGINE = InnoDB;

-- ここから上をカテゴリー/タグで管理しようと思ったけど,
-- グループ/xx会/xx作者 のような感じで、グループは一つ上の親を必要とするので代替不可能
-- タグだと親子関係までだから、3世代になると難しい

CREATE TABLE IF NOT EXISTS books (
  id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  book_code      VARCHAR(255)   NOT NULL,
  book_name    VARCHAR(255)   NOT NULL,
  creator_id    INT   NOT NULL,
  uploaded_at DATETIME NOT NULL,
  FOREIGN KEY(creator_id) REFERENCES creators(id)
) ENGINE = InnoDB;

CREATE TABLE IF NOT EXISTS images (
  id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  image_code      VARCHAR(255)   NOT NULL,
  image_name    VARCHAR(255)   NOT NULL,
  image_url    VARCHAR(255)   NOT NULL,
  book_id    INT   NOT NULL,
  uploaded_at DATETIME NOT NULL,
  FOREIGN KEY(book_id) REFERENCES books(id)
) ENGINE = InnoDB;

CREATE TABLE IF NOT EXISTS categories (
  id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  category_code      VARCHAR(255)   NOT NULL,
  category_name    VARCHAR(255)   NOT NULL,
  category_color    VARCHAR(255)   NOT NULL
) ENGINE = InnoDB;

CREATE TABLE IF NOT EXISTS tags (
  id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  tag_code      VARCHAR(255)   NOT NULL,
  tag_name    VARCHAR(255)   NOT NULL,
  category_id      INT NOT NULL,
  FOREIGN KEY(category_id) REFERENCES categories(id)
) ENGINE = InnoDB;

CREATE TABLE IF NOT EXISTS books_tags(
  id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  book_id INT NOT NULL,
  tag_id INT NOT NULL
) ENGINE = InnoDB;

CREATE TABLE IF NOT EXISTS images_tags(
  id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  image_id INT NOT NULL,
  tag_id INT NOT NULL
) ENGINE = InnoDB;



-- CREATE DATABASE IF NOT EXISTS employees;
-- USE employees;

-- CREATE TABLE employees (
--   emp_no      INT             NOT NULL,
--   birth_date  DATE            NOT NULL,
--   first_name  VARCHAR(14)     NOT NULL,
--   last_name   VARCHAR(16)     NOT NULL,
--   gender      ENUM ('M','F')  NOT NULL,
--   hire_date   DATE            NOT NULL,
--   PRIMARY KEY (emp_no)
-- );