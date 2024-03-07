import { useEffect, useRef } from "react";
import { Flex, Button, Box, Card } from "@radix-ui/themes";
import * as ScrollArea from "@radix-ui/react-scroll-area";
import "./styles.css";

import ImageCard from "@ui/media/ImageCard";

const items = [
  {
    id: "1",
    color: "red",
    url: "https://images.unsplash.com/photo-1551963831-b3b1ca40c98e",
    text: "Breakfast",
  },
  {
    id: "2",
    color: "red",
    url: "https://images.unsplash.com/photo-1551782450-a2132b4ba21d",
    text: "Burger",
  },
  {
    id: "3",
    color: "red",
    url: "https://images.unsplash.com/photo-1522770179533-24471fcdba45",
    text: "Camera",
  },
  {
    id: "4",
    color: "red",
    url: "https://images.unsplash.com/photo-1444418776041-9c7e33cc5a9c",
    text: "Coffee",
  },
  {
    id: "5",
    color: "red",
    url: "https://images.unsplash.com/photo-1533827432537-70133748f5c8",
    text: "Hats",
  },
  {
    id: "6",
    color: "red",
    url: "https://images.unsplash.com/photo-1558642452-9d2a7deb7f62",
    text: "Honey",
  },
  {
    id: "7",
    color: "red",
    url: "https://images.unsplash.com/photo-1516802273409-68526ee1bdd6",
    text: "Basketball",
  },
  {
    id: "8",
    color: "red",
    url: "https://images.unsplash.com/photo-1518756131217-31eb79b20e8f",
    text: "Fern",
  },
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
];

function VerticalScroll() {
  const scrollViewportRef = useRef(null);
  const addHandler = (scrollElement: HTMLElement) =>
    scrollElement.addEventListener("wheel", (event) => {
      const e = event as WheelEvent;
      if (Math.abs(e.deltaY) < Math.abs(e.deltaX)) return;

      // const maxScrollLeft = scrollElement.scrollWidth - scrollElement.clientWidth;

      // if (
      //   (scrollElement.scrollLeft <= 0 && e.deltaY < 0) ||
      //   (scrollElement.scrollLeft >= maxScrollLeft && e.deltaY > 0)
      // )
      //   return;

      e.preventDefault();
      scrollElement.scrollLeft += e.deltaY / 1.7;
    });

  const removeHandler = (scrollElement: HTMLElement) =>
    scrollElement.removeEventListener("wheel", () => {});

  useEffect(() => {
    const elem = scrollViewportRef.current;
    if (!elem) return () => {};
    addHandler(elem);
    return removeHandler(elem);
  }, []);

  const imageCards = items.map((it) => <ImageCard key={it.id} text={it.text} url={it.url} />);

  return (
    <ScrollArea.Root className={"ScrollAreaRoot"}>
      <ScrollArea.Viewport ref={scrollViewportRef} className={"ScrollAreaViewport"}>
        <Flex direction={"row"} gap={"3"}>
          {/* <div className={"Text"}>Tags</div>
          {TAGS.map((tag) => (
            <div className={"Tag"} key={tag}>
              {tag}
            </div>
          ))} */}
          {imageCards}
        </Flex>
      </ScrollArea.Viewport>
      {/* <ScrollArea.Scrollbar className={"ScrollAreaScrollbar"} orientation={"horizontal"}>
        <ScrollArea.Thumb className={"ScrollAreaThumb"} />
      </ScrollArea.Scrollbar> */}
    </ScrollArea.Root>
  );
}

export default VerticalScroll;
