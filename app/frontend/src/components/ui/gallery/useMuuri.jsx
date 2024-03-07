import React, { useState, useEffect } from "react";
import Muuri from "muuri";
import "./muuri.css";

import GridGalleryItem from "./GridGalleryItem";

const useMuuri = ({ initItems, onClick }) => {
  const [items, setItems] = useState(initItems);
  const [grid, setGrid] = useState();

  useEffect(() => {
    setGrid(
      new Muuri(".grid", {
        // showDuration: 3000,
        // showEasing: "ease",
        dragEnabled: true,
        layoutDuration: 400,
        layoutEasing: "ease",
        fillGaps: true,
        horizontal: true,
        sortData: {
          tag: (item, element) => {
            // console.log(item);
            console.log(element.children);
            // console.log(element.children[0].id);
            // 数字を文字列のまま渡してしまうとunicode順の表示になるので、number型に変換する
            // return parseInt(element.children[0].id, 10);
            return element.getAttribute("data-tag");
          },
        },
      }),
    );
  }, []);

  const add = (newItems) => {
    // console.log("length", items.length);
    // console.log("追加する要素の番号", items.length + 1);
    setItems((prevItems) => [
      ...prevItems,
      ...newItems.map((it) => ({
        ...it,
        ref: (element) => {
          // requestAnimationFrame(() => {
          grid.add([element]);
          // });
        },
      })),
      // {
      //   id: items.length + 1,
      //   text: items.length + 1,
      //   ref: (element) => {
      //     grid.add([element]);
      //   },
      // },
    ]);
  };

  const sort = (label) => grid.sort(label);

  const filter = (label) => grid.filter(label);

  const handleClick = (event) => {
    const itemId = event.currentTarget.id;
    onClick(itemId);
  };

  const gridGalleryItems = (its) => {
    console.log("要素数", its.length);
    return (
      <>
        {its.map((it) => (
          <GridGalleryItem
            key={it.id}
            id={it.id}
            text={it.text}
            url={it.url}
            handleClick={handleClick}
            ref={it.ref}
          />
        ))}
      </>
    );
  };

  const muuri = <div className="grid">{gridGalleryItems(items)}</div>;

  return { muuri, add, sort, filter };
};
export default useMuuri;
