import { useState, useEffect, ChangeEvent } from "react";
import { ReactSortable } from "react-sortablejs";
import { AnimatePresence, motion, LayoutGroup } from "framer-motion";
import { Box, Button, Switch, FormControlLabel } from "@mui/material";

import "./style.css";

const initItems = [
  {
    id: "1",
    tags: ["1", "star", "3d"],
    color: "blue",
    img: "http://localhost:9000/sample/1741410963554025670/3_1741409951392923648/GCq7H9kbAAAznjt.png",
    title: "Breakfast",
  },
  {
    id: "2",
    tags: ["2", "", "3d"],
    color: "white",
    img: "https://images.unsplash.com/photo-1551782450-a2132b4ba21d",
    title: "Burger",
  },
  {
    id: "3",
    tags: ["3", "star", ""],
    color: "red",
    img: "https://images.unsplash.com/photo-1522770179533-24471fcdba45",
    title: "Camera",
  },
  {
    id: "4",
    tags: ["2", "", ""],
    color: "blue",
    img: "https://images.unsplash.com/photo-1444418776041-9c7e33cc5a9c",
    title: "Coffee",
  },
  {
    id: "5",
    tags: ["1", "star", ""],
    color: "blue",
    img: "https://images.unsplash.com/photo-1533827432537-70133748f5c8",
    title: "Hats",
  },
  {
    id: "6",
    tags: ["0", "", "3d"],
    color: "blue",
    img: "https://images.unsplash.com/photo-1558642452-9d2a7deb7f62",
    title: "Honey",
  },
  {
    id: "7",
    tags: ["0", "star", "3d"],
    color: "red",
    img: "https://images.unsplash.com/photo-1516802273409-68526ee1bdd6",
    title: "Basketball",
  },
  {
    id: "8",
    tags: ["1", "", ""],
    color: "blue",
    img: "https://images.unsplash.com/photo-1518756131217-31eb79b20e8f",
    title: "Fern",
  },
  {
    id: "9",
    tags: ["2", "star", ""],
    color: "blue",
    img: "https://images.unsplash.com/photo-1597645587822-e99fa5d45d25",
    title: "Mushrooms",
  },
  {
    id: "10",
    tags: ["", "", "3d"],
    color: "blue",
    img: "https://images.unsplash.com/photo-1567306301408-9b74779a11af",
    title: "Tomato basil",
  },
  {
    id: "11",
    tags: ["2", "star", ""],
    color: "blue",
    img: "https://images.unsplash.com/photo-1471357674240-e1a485acb3e1",
    title: "Sea star",
  },
  {
    id: "12",
    tags: ["3", "", "3d"],
    color: "red",
    img: "https://images.unsplash.com/photo-1589118949245-7d38baf380d6",
    title: "Bike",
  },
];

const variants = {
  visible: { opacity: 1 },
  hidden: { opacity: 0 },
};

export type Item = {
  // ReactSortableでidは必須
  id: string;
  tags: string[];
  color: string;
  img: string;
  title: string;
};

function GridSort() {
  const [isAllowAnimation, setIsAllowAnimation] = useState(true);
  const [items, setItems] = useState(initItems);

  // 要素が並び終わった後にlayout対象をtrueにする
  // useEffect(() => {
  //   setIsAllowAnimation(true);
  // }, [items]);
  // Drag and Drop Handler
  const onDragDropEnds = (oldIndex, newIndex) => {
    console.log("Drag and drop other tasks");
    console.log(oldIndex, newIndex);
    // ドラッグ&ドロップした後に、framer-motionのlayout対象をfalseにすることで
    // 並び替えのアニメーションを無効にする
    // setIsAllowAnimation(false);
  };

  const onClickButton = () => {
    // setItems([]);
    setItems((prevItems) => {
      const newItems = prevItems.filter((it) => it.color === "red");
      console.log("newItems", newItems);
      return newItems;
    });
  };

  return (
    <Box>
      <FormControlLabel
        control={
          <Switch
            checked={isAllowAnimation}
            onChange={(e: ChangeEvent<HTMLInputElement>) => setIsAllowAnimation(e.target.checked)}
          />
        }
        label={"モード切り替え"}
      />
      <Button onClick={onClickButton}>整列</Button>
      <ReactSortable
        animation={150}
        swapThreshold={0.5}
        direction={"horizontal"}
        // fallbackOnBody
        // invertSwap
        list={items}
        setList={(newlist) => setItems(newlist)}
        ghostClass={"dropArea"}
        handle={".dragHandle"}
        // filter={".ignoreDrag"}
        // preventOnFilter
        className={"grid-container"}
        onChoose={({ oldIndex, newIndex }) => onDragDropEnds(oldIndex, newIndex)}
      >
        <AnimatePresence mode={"popLayout"}>
          {items.map((it) => (
            <motion.div
              key={it.id}
              // sortable.jsでドラッグ可能な要素に「dragHandle」cssクラスを付与する
              className={"dragHandle"}
              layout={isAllowAnimation}
              initial={{ scale: 0.8, opacity: 0 }}
              animate={{ scale: 1, opacity: 1 }}
              exit={{ scale: 0.8, opacity: 0 }}
              // 初期表示時のみ以下のアニメーションにしたい
              // transition={{ ease: "easeOut", duration: 2 }}
              // ホバー時のアニメーション
              whileHover={isAllowAnimation ? { scale: [null, 1.5, 1.4] } : undefined}
              transition={isAllowAnimation ? { duration: 0.3 } : undefined}
            >
              <img
                key={it.img}
                // srcSet={`${img}?w=100&h=100&fit=crop&auto=format&dpr=2 2x`}
                src={`${it.img}?w=200&h=200&fit=crop&auto=format`}
                alt={it.title}
                loading={"lazy"}
              />
            </motion.div>
          ))}
        </AnimatePresence>
      </ReactSortable>
    </Box>
  );
}

export default GridSort;
