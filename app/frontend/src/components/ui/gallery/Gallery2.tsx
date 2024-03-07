import React, { useEffect, useRef } from "react";
import Muuri from "muuri";
import "web-animations-js"; // Web Animations API polyfill
import { ImageList, ImageListItem } from "@mui/material";

function MuuriGrid() {
  // grid要素への参照を保持します
  const gridRef = useRef(null);

  // コンポーネントがマウントされたら、Muuriグリッドを初期化します
  useEffect(() => {
    // gridRef.currentがDOM要素を指していることを確認します
    if (!gridRef.current) {
      return;
    }

    // Muuriグリッドの新しいインスタンスを作成します
    const grid = new Muuri(gridRef.current, {
      // ここでオプションを設定します（例：レイアウト、ドラッグの許可等）
      dragEnabled: true, // 例：アイテムのドラッグを有効にします
      layout: {
        // この例では、レイアウトオプションはデフォルトのままにしています
      },
    });

    // コンポーネントがアンマウントされたときにグリッドを破棄します
    return () => {
      grid.destroy();
    };
  }, []); // 空の依存配列は、このエフェクトをコンポーネントのマウント/アンマウント時にのみ実行することを意味します

  return (
    <div ref={gridRef} className={"grid"}>
      <ImageList sx={{ width: 500, height: 450 }} cols={3} rowHeight={164}>
        <ImageListItem>
          <div className={"item"}>
            <div className={"item-content"}>
              <img
                // srcSet={`${item.img}?w=164&h=164&fit=crop&auto=format&dpr=2 2x`}
                src={`https://images.unsplash.com/photo-1551963831-b3b1ca40c98e?w=164&h=164&fit=crop&auto=format`}
                alt={"Breakfast"}
                // loading="lazy"
              />
            </div>
          </div>
        </ImageListItem>
        <ImageListItem>
          <div className={"item"}>
            <div className={"item-content"}>
              <img
                // srcSet={`${item.img}?w=164&h=164&fit=crop&auto=format&dpr=2 2x`}
                src={`https://images.unsplash.com/photo-1551963831-b3b1ca40c98e?w=164&h=164&fit=crop&auto=format`}
                alt={"Breakfast"}
                // loading="lazy"
              />
            </div>
          </div>
        </ImageListItem>
        <ImageListItem>
          <div className={"item"}>
            <div className={"item-content"}>
              <img
                // srcSet={`${item.img}?w=164&h=164&fit=crop&auto=format&dpr=2 2x`}
                src={`https://images.unsplash.com/photo-1551963831-b3b1ca40c98e?w=164&h=164&fit=crop&auto=format`}
                alt={"Breakfast"}
                // loading="lazy"
              />
            </div>
          </div>
        </ImageListItem>
      </ImageList>
    </div>
  );
}

export default MuuriGrid;
