import { Box, AspectRatio } from "@radix-ui/themes";

type ImageCardProps = {
  // id: string;
  text: string;
  url: string;
  // handleClick: () => void;
};

function ImageCard({ text, url }: ImageCardProps) {
  return (
    <Box
      style={{
        width: "225px",
        height: "225px",
      }}
    >
      <AspectRatio>
        <img
          src={url}
          alt={text}
          style={{
            objectFit: "cover",
            width: "100%",
            height: "100%",
            borderRadius: "var(--radius-2)",
          }}
        />
      </AspectRatio>
    </Box>
  );
}

export default ImageCard;
