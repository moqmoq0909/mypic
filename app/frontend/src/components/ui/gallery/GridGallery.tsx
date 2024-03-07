import { Box, Button, TextField } from "@mui/material";

import useMuuri from "./useMuuri";

type MuuriItem = {
  id: number;
  text: string;
  url: string;
  ref: () => void;
};

function GridGallery() {
  const items: MuuriItem[] = [
    {
      id: 10,
      text: "a",
      url: "https://images.unsplash.com/photo-1551963831-b3b1ca40c98e",
      ref: () => {},
    },
    {
      id: 20,
      text: "b",
      url: "https://images.unsplash.com/photo-1551782450-a2132b4ba21d",
      ref: () => {},
    },
    {
      id: 30,
      text: "c",
      url: "https://images.unsplash.com/photo-1522770179533-24471fcdba45",
      ref: () => {},
    },
    {
      id: 40,
      text: "d",
      url: "https://images.unsplash.com/photo-1444418776041-9c7e33cc5a9c",
      ref: () => {},
    },
    {
      id: 50,
      text: "e",
      url: "https://images.unsplash.com/photo-1533827432537-70133748f5c8",
      ref: () => {},
    },
    {
      id: 60,
      text: "f",
      url: "https://images.unsplash.com/photo-1558642452-9d2a7deb7f62",
      ref: () => {},
    },
    {
      id: 70,
      text: "g",
      url: "https://images.unsplash.com/photo-1516802273409-68526ee1bdd6",
      ref: () => {},
    },
  ];

  const subItemsMap = new Map<number, MuuriItem[]>([
    [
      10,
      [
        {
          id: 11,
          text: "a",
          url: "https://images.unsplash.com/photo-1551963831-b3b1ca40c98e",
          ref: () => {},
        },
        {
          id: 12,
          text: "a",
          url: "https://images.unsplash.com/photo-1551963831-b3b1ca40c98e",
          ref: () => {},
        },
      ],
    ],
    [
      20,
      [
        {
          id: 21,
          text: "b",
          url: "https://images.unsplash.com/photo-1551782450-a2132b4ba21d",
          ref: () => {},
        },
        {
          id: 22,
          text: "b",
          url: "https://images.unsplash.com/photo-1551782450-a2132b4ba21d",
          ref: () => {},
        },
      ],
    ],
  ]);

  console.log("subItemsMap", subItemsMap);

  const { muuri, add, sort, filter } = useMuuri({
    initItems: items,
    onClick: (idStr: string) => {
      console.log("id", idStr);
      const id = parseInt(idStr, 10);
      const subItems = subItemsMap.get(id);
      console.log("subItems", subItems);
      if (subItems) add(subItems);
    },
  });

  return (
    <Box sx={{ margin: "0 auto", width: "930px" }}>
      <Button variant={"outlined"} onClick={() => sort("tag")}>
        昇順
      </Button>
      <Button variant={"outlined"} onClick={() => sort("tag:desc")}>
        降順
      </Button>
      <TextField variant={"outlined"} />
      <Button variant={"outlined"} onClick={() => filter("[data-color]")}>
        フィルター
      </Button>
      <Button variant={"outlined"} onClick={add}>
        追加
      </Button>
      {muuri}
    </Box>
  );
}

export default GridGallery;
