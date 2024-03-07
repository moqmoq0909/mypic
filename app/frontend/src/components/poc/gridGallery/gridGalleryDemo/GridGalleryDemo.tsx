import { useState } from "react";
import { MuuriComponent } from "muuri-react";

import Beetle from "@/assets/beetle.jpeg";
import Dog from "@/assets/dog.jpeg";
import Kroeber from "@/assets/kroeber.jpeg";
import Lion from "@/assets/lion.png";
import Whisky from "@/assets/whisky.jpeg";

const initialItems = [
  { id: 1, src: Beetle },
  { id: 2, src: Dog },
  { id: 3, src: Kroeber },
  { id: 4, src: Lion },
  { id: 5, src: Whisky },
];

function GridGalleryDemo() {
  const [items, setItems] = useState(initialItems);

  // img要素がロードされた時にmuuri-item要素に画像のサイズをセット
  const handleOnLoad = (e: React.SyntheticEvent<HTMLImageElement, Event>) => {
    const imageElement = e.target as HTMLImageElement;
    const parentElement = imageElement.parentNode as HTMLElement | null;
    if (parentElement) {
      console.log("画像の高さ:", imageElement.height, "px");
      console.log("画像の幅:", imageElement.width, "px");
      parentElement.style.height = `${imageElement.height / 2}px`;
      parentElement.style.width = `${imageElement.width / 2}px`;
      imageElement.style.height = `${imageElement.height / 2}px`;
      imageElement.style.width = `${imageElement.width / 2}px`;
    }
  };

  return (
    <div>
      <MuuriComponent>
        {items.map((item) => (
          <div key={item.id} className={"muuri-item"}>
            <img src={item.src} alt={""} onLoad={handleOnLoad} />
          </div>
        ))}
      </MuuriComponent>
    </div>
  );
}
export default GridGalleryDemo;
