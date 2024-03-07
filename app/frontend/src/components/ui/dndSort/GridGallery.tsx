/** @jsxImportSource @emotion/react */
// import { HTMLAttributes } from "react";
import { css } from "@emotion/react";
// import { Box, Button, TextField } from "@mui/material";

import useDnDSort from "./useDnDSort";

// type Style<T extends HTMLElement> = HTMLAttributes<T>["style"];

const bodyStyle = css({
  width: "930px",
  margin: "0 auto",
});

const containerStyle = css({
  display: "flex",
  flexWrap: "wrap",
  justifyContent: "space-between",
  /*
    justify-content: space-betweenの最後の行を左寄せにしたい時の対処方法
    https://webtan.tech/space-between-lastline/
  */
  "&:after": {
    display: "block",
    content: `""`,
    width: "310px", // imageCardのマージンも含めたwidthの値
  },
});

const imageCardStyle = css({
  width: "300px", // 画像の見た目のサイズになるため、比率を考えておく
  height: "300px", // 画像の見た目のサイズになるため、比率を考えておく

  cursor: "grab", // マウスを手のマークに変える
  userSelect: "none", // 対象を選択範囲として選べないよう設定
  overflow: "hidden", // はみ出た要素は非表示
  borderRadius: "5px",
  margin: "5px",
});

const imageStyle = css({
  pointerEvents: "none", // マウスのポインタイベントの対象になるかを制御、親要素でgrabにしているのでこちらではnone?
  objectFit: "cover", // 親要素に対して、画像をどのようにはめ込むかを指定している
  width: "100%",
  height: "100%",
});

const imageList: string[] = [
  "https://images.unsplash.com/photo-1551963831-b3b1ca40c98e",
  "https://images.unsplash.com/photo-1551782450-a2132b4ba21d",
  "https://images.unsplash.com/photo-1522770179533-24471fcdba45",
];

function GridGallery() {
  // useDnDSort()を使って並び替え処理を実装する
  const { results, setItems } = useDnDSort(imageList);
  const addItem = () => {
    setItems((prevItems) => [
      ...prevItems,
      "https://images.unsplash.com/photo-1444418776041-9c7e33cc5a9c",
      "https://images.unsplash.com/photo-1533827432537-70133748f5c8",
      "https://images.unsplash.com/photo-1558642452-9d2a7deb7f62",
      "https://images.unsplash.com/photo-1516802273409-68526ee1bdd6",
    ]);
  };
  return (
    <div css={bodyStyle}>
      <button type={"button"} onClick={addItem}>
        要素追加
      </button>
      <div css={containerStyle}>
        {results.map((item) => (
          <div key={item.key} css={imageCardStyle} {...item.events}>
            <img src={item.value} alt={"ソート可能な画像"} css={imageStyle} />
          </div>
        ))}
      </div>
    </div>
  );
}
export default GridGallery;
