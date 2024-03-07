import { useState } from "react";
import { MuuriComponent } from "muuri-react";

import { Flex, Button, Box } from "@radix-ui/themes";

import "./muuri-core.css";
import { useFilter, generateItems, options, itemData } from "./utils";

import GalleryItem from "./GalleryItem";

import PullDownSelect from "../input/pullDownSelect/PullDownSelect";
import Switch from "../input/switch/Switch";
import TextField from "../input/textField/TextField";

function Gallery() {
  const [items, setItems] = useState(itemData);

  const children = items.map((it) => <GalleryItem key={it.id} text={it.text} url={it.url} />);

  const add = () => {
    setItems((prevItems) => [
      ...prevItems,
      {
        id: "9",
        color: "red",
        url: "https://images.unsplash.com/photo-1597645587822-e99fa5d45d25",
        text: "Mushrooms",
      },
      {
        id: "10",
        color: "red",
        url: "https://images.unsplash.com/photo-1567306301408-9b74779a11af",
        text: "Tomato basil",
      },
      {
        id: "11",
        color: "red",
        url: "https://images.unsplash.com/photo-1471357674240-e1a485acb3e1",
        text: "Sea star",
      },
      {
        id: "12",
        color: "red",
        url: "https://images.unsplash.com/photo-1589118949245-7d38baf380d6",
        text: "Bike",
      },
    ]);
  };

  const [isEditable, setIsEditable] = useState<boolean>(false);

  const editMode = () => setIsEditable((prev) => !prev);

  return (
    // <Box sx={{ width: "930px", height: "100vh", margin: "100px auto 0" }}>
    <Box style={{ width: "930px", height: "100vh", margin: "100px auto 0" }}>
      {/* <Button variant={"outlined"} onClick={() => sort("tag")}>
        昇順
      </Button>
      <Button variant={"outlined"} onClick={() => sort("tag:desc")}>
        降順
      </Button>
      <TextField variant={"outlined"} />
      <Button variant={"outlined"} onClick={() => filter("[data-color]")}>
        フィルター
      </Button> */}
      <Flex direction={"row"}>
        <TextField />
        <Button size={"2"} onClick={add}>
          追加
        </Button>
        <Switch onClick={editMode} text={"編集モード"} />
        <PullDownSelect />
      </Flex>
      <MuuriComponent
        // dragSortHeuristics={{
        //   sortInterval: 70,
        // }}
        // layoutDuration={400}
        // dragRelease={{
        //   duration: 400,
        //   easing: "ease-in",
        // }}
        dragEnabled={isEditable}
        // dragContainer={document.body}
        // dragPlaceholder={{
        //   enabled: true,
        //   createElement: (item) => item.getElement().cloneNode(true),
        // }}
        // {...options}
        // propsToData={({ color, title }) => ({ color, title })}
        // filter={filterFunction}
        // sort={sort.value}
      >
        {children}
      </MuuriComponent>
    </Box>
  );
}
export default Gallery;
