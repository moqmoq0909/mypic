// ★emotionとmuuri-reactの相性が悪いのでemotionは使わない
import "./gallery-item.css";

type GridGalleryItemProps = {
  // id: string;
  text: string;
  url: string;
  // handleClick: () => void;
};

function GalleryItem({ text, url }: GridGalleryItemProps) {
  return (
    <div className={"item"}>
      <div className={"item-content"}>
        <div className={"card"}>
          <img className={"image"} src={url} alt={text} loading={"lazy"} />
        </div>
      </div>
    </div>
  );
}

export default GalleryItem;
