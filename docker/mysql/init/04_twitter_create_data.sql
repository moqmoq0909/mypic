USE tags;

INSERT INTO creators VALUES 
(1, 'creator_1', 'muimui', '{
  "group": "飛翔社",
  "sns": {"x":"x.com", "pixiv":"pixiv.com"}
}'),
(2, 'creator_2', 'ひまたろー', '{
  "group": "ガジェットフリーク株式会社",
  "sns": {"x":"x.com"}
}'),
(3, 'creator_3', '藤井俊介', '{
  "group": "ガジェットフリーク株式会社"
}'),
(4, 'creator_4', 'exoxe', '{
  "sns": {"x":"x.com"}
}'),
(5, 'creator_5', 'さまさまサマー', NULL);

INSERT INTO books VALUES 
(1, 'book_1', NULL, 'ツイート内容など', 0, 'comic', '[
    {
      "imageId": 1,
      "imageKey": "xxx",
      "imageName": "01",
      "imageUrl": "xxx.com",
      "imageType": "image"
    },
    {
      "imageId": 2,
      "imageKey": "yyy",
      "imageName": "02",
      "imageUrl": "yyy.com",
      "imageType": "video"
    }
]', 5, now(), now()),
(2, 'book_2', NULL, NULL, 0, 'twitter', '[
    {
      "imageId": 3,
      "imageKey": "zzz",
      "imageName": "01",
      "imageUrl": "zzz.com",
      "imageType": "image"
    }
]', 3, now(), now()),  -- 日付の形式を修正
(3, 'book_3', NULL, NULL, 0, 'comic', '[
    {
      "imageId": 4,
      "imageKey": "aaa",
      "imageName": "01",
      "imageUrl": "aaa.com",
      "imageType": "image"
    },
    {
      "imageId": 5,
      "imageKey": "aaa",
      "imageName": "02",
      "imageUrl": "aaa.com",
      "imageType": "image"
    }
]', 2, now(), now()),
(4, 'book_4', NULL, NULL, 1, 'twitter', '[
    {
      "imageId": 6,
      "imageKey": "aaa",
      "imageName": "01",
      "imageUrl": "aaa.com",
      "imageType": "image"
    }
]', 2, now(), now()),
(5, 'book_5', NULL, NULL, 1, 'comic', '[
    {
      "imageId": 7,
      "imageKey": "aaa",
      "imageName": "01",
      "imageUrl": "aaa.com",
      "imageType": "image"
    }
]', 5, now(), now());

INSERT INTO categories VALUES (1,"category_1","未分類","#ff0000"),
(2,"category_2","自分用","#98fb98"),
(3,"category_3","少年漫画","#6495ED");

INSERT INTO tags VALUES (1,"tag_1","オススメ",2),
(2,"tag_2","激アツ",3),
(3,"tag_3","教養",1),
(4,"tag_4","中2",3),
(5,"tag_5","お気に入り",2);

INSERT INTO books_tags VALUES (1,4,1,3),
(2,4,4,2),
(3,3,2,2),
(4,3,4,2),
(5,2,3,2),
(6,5,1,2),
(7,5,2,2),
(8,5,3,2);



-- USE employees;

-- INSERT INTO `employees` VALUES (10001,"1953-09-02","Georgi","Facello","M","1986-06-26"),
-- (10002,"1964-06-02","Bezalel","Simmel","F","1985-11-21"),
-- (10003,"1959-12-03","Parto","Bamford","M","1986-08-28"),
-- (10004,"1954-05-01","Chirstian","Koblick","M","1986-12-01"),
-- (10005,"1955-01-21","Kyoichi","Maliniak","M","1989-09-12"),
-- (10006,"1953-04-20","Anneke","Preusig","F","1989-06-02"),
-- (10007,"1957-05-23","Tzvetan","Zielinski","F","1989-02-10"),
-- (10008,"1958-02-19","Saniya","Kalloufi","M","1994-09-15"),
-- (10009,"1952-04-19","Sumant","Peac","F","1985-02-18"),
-- (10010,"1963-06-01","Duangkaew","Piveteau","F","1989-08-24");