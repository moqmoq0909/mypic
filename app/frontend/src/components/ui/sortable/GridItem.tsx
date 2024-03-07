import { motion } from "framer-motion";

import type { Item } from "./GridSort";

type GridItemProps = {
  item: Item;
};

const variants = {
  visible: { opacity: 1 },
  hidden: { opacity: 0 },
};

function GridItem({ item }: GridItemProps) {
  return (
    <motion.div initial={"hidden"} animate={"visible"} variants={variants}>
      <div className={"dragHandle"}>
        <img
          key={item.img}
          // srcSet={`${img}?w=100&h=100&fit=crop&auto=format&dpr=2 2x`}
          src={`${item.img}?w=200&h=200&fit=crop&auto=format`}
          alt={item.title}
          loading={"lazy"}
        />
      </div>
    </motion.div>
  );
}

export default GridItem;
