/** @jsxImportSource @emotion/react */
import { forwardRef } from "react";
import { css } from "@emotion/react";

type GridGalleryItemProps = {
  id: string;
  text: string;
  url: string;
  handleClick: () => void;
};

function GridGalleryItem({ id, text, url, handleClick }: GridGalleryItemProps, ref: any) {
  const itemStyle = css({
    margin: "5px",
  });

  const itemContentStyle = css({});

  const imageCardStyle = css({
    width: "300px",
    height: "300px",

    // 見た目に関するcss
    userSelect: "none", // 対象を選択範囲として選べないよう設定
    overflow: "hidden", // はみ出た要素は非表示
    cursor: "grab",
    borderRadius: "5px",
  });

  const imageStyle = css({
    width: "100%",
    height: "100%",

    // 見た目用のcss
    objectFit: "cover", // 親要素に対して、画像をどのようにはめ込むかを指定している
  });

  return (
    <div id={id} className={"item"} ref={ref} data-tag={text} css={itemStyle} onClick={handleClick}>
      <div className={"item-content"} css={itemContentStyle}>
        <div css={imageCardStyle}>
          <img src={url} alt={text} css={imageStyle} />
        </div>
      </div>
    </div>
  );
}
export default forwardRef<HTMLDivElement, GridGalleryItemProps>(GridGalleryItem);
