type Image = {
  imageId: string; // MySQLで管理しないので必要ないかも
  imageKey: string;
  imageName: string;
  imageUrl: string;
  imageType: "image" | "video";
};

// JSONの構造は特に決めていない、例えば以下のような感じ
type Profile = {
  group: string;
  sns: {};
};
