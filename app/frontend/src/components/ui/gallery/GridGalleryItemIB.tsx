/** @jsxImportSource @emotion/react */
import { forwardRef } from "react";
import { css } from "@emotion/react";

type GridGalleryItemProps = {
  id: string;
  text: string;
  url: string;
};

function GridGalleryItem({ id, text, url }: GridGalleryItemProps, ref: any) {
  const itemStyle = css({
    margin: "5px",
  });

  const itemContentStyle = css({
    width: "300px",
    height: "300px",

    // アスペクト比用のcss
    // width: "calc(100vw / 3.1)",

    // 見た目に関するcss
    // background: "black",
    backgroundImage: `url(${url})`,
    backgroundSize: "cover",

    // display: "flex",
    // justifyContent: "center",
    // alignItems: "center",
    cursor: "grab",
    borderRadius: "5px",
  });

  const backdropFilterStyle = css({
    // webkitBackdropFilter: "blur(10px)",
    backdropFilter: "blur(10px) saturate(70%)",

    width: "100%",
    height: "100%",
    display: "flex",
    justifyContent: "center",
    alignItems: "center",
    borderRadius: "5px",
  });

  const imageCardStyle = css({
    width: "calc(300px / 1.41279)",
    height: "300px",

    // アスペクト比用のcss
    // position: "relative",
    // width: "100%",
    // paddingTop: "141.279%", // B5のアスペクト比

    // 見た目に関するcss
    // cursor: "grab", // マウスを手のマークに変える
    userSelect: "none", // 対象を選択範囲として選べないよう設定
    overflow: "hidden", // はみ出た要素は非表示
    // borderRadius: "5px",
    // margin: 3,
  });

  const imageStyle = css({
    width: "100%",
    height: "100%",

    // アスペクト比用のcss
    // position: "absolute",
    // top: "0",
    // left: "0",
    // width: "100%",
    // height: "100%",

    // 見た目用のcss
    // pointerEvents: "none", // マウスのポインタイベントの対象になるかを制御、親要素でgrabにしているのでこちらではnone?
    objectFit: "cover", // 親要素に対して、画像をどのようにはめ込むかを指定している
  });

  return (
    <div className={"item"} ref={ref} data-tag={text} css={itemStyle}>
      <div className={"item-content"} css={itemContentStyle}>
        <div css={backdropFilterStyle}>
          <div css={imageCardStyle}>
            <img src={url} alt={text} css={imageStyle} />
          </div>
        </div>
      </div>
    </div>
  );
}
export default forwardRef<HTMLDivElement, GridGalleryItemProps>(GridGalleryItem);
