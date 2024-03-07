USE mypic;

INSERT INTO creator_groups VALUES (1,'cg_1','藤井俊介'),
(2,'cg_2','飛翔社'),
(3,'cg_3','ガジェットフリーク株式会社'),
(4,'cg_4','大東京出版'),
(5,'cg_5','快活工房');

INSERT INTO creators VALUES (1,'creator_1','muimui',5),
(2,'creator_2','ひまたろー',2),
(3,'creator_3','藤井俊介',1),
(4,'creator_4','exoxe',2),
(5,'creator_5','さまさまサマー',2);

INSERT INTO books VALUES (1,'book_1','9月は夏です 〜暑すぎてヤヴァイ〜',5,'2022-09-01'),
(2,'book_2','ラーメン放浪記 2',3,'2023-04-8'),
(3,'book_3','隣の忍者 激闘編',2,'2023-12-24'),
(4,'book_4','猫に転生した犬',2,'2023-06-20'),
(5,'book_5','東京鬼ごっこ re:start',5,'2023-02-15');

INSERT INTO images VALUES (1,'image_1','01','aws.s3.xxxxxxxx',2,'2022-09-01'),
(2,'image_2','02','aws.s3.xxxxxxxx',2,'2023-04-8'),
(3,'image_3','03','aws.s3.xxxxxxxx',2,'2023-12-24'),
(4,'image_4','04','aws.s3.xxxxxxxx',2,'2023-06-20'),
(5,'image_5','05','aws.s3.xxxxxxxx',2,'2023-02-15');

INSERT INTO categories VALUES (1,'category_1','未分類','#ff0000'),
(2,'category_2','自分用','#98fb98'),
(3,'category_3','少年漫画','#6495ED');

INSERT INTO tags VALUES (1,'tag_1','オススメ',2),
(2,'tag_2','激アツ',3),
(3,'tag_3','教養',1),
(4,'tag_4','中2',3),
(5,'tag_5','お気に入り',2);

INSERT INTO books_tags VALUES (1,4,1),
(2,4,4),
(3,3,2),
(4,3,4),
(5,2,3),
(6,5,1),
(7,5,2),
(8,5,3);



-- USE employees;

-- INSERT INTO `employees` VALUES (10001,'1953-09-02','Georgi','Facello','M','1986-06-26'),
-- (10002,'1964-06-02','Bezalel','Simmel','F','1985-11-21'),
-- (10003,'1959-12-03','Parto','Bamford','M','1986-08-28'),
-- (10004,'1954-05-01','Chirstian','Koblick','M','1986-12-01'),
-- (10005,'1955-01-21','Kyoichi','Maliniak','M','1989-09-12'),
-- (10006,'1953-04-20','Anneke','Preusig','F','1989-06-02'),
-- (10007,'1957-05-23','Tzvetan','Zielinski','F','1989-02-10'),
-- (10008,'1958-02-19','Saniya','Kalloufi','M','1994-09-15'),
-- (10009,'1952-04-19','Sumant','Peac','F','1985-02-18'),
-- (10010,'1963-06-01','Duangkaew','Piveteau','F','1989-08-24');