import { KeyboardArrowRight, KeyboardArrowDown } from "@mui/icons-material";

type AccordionBtnType = {
  isOpen: boolean;
  onClick: (isOpen: boolean) => void;
};

export default function AccordionBtn({ isOpen, onClick }: AccordionBtnType) {
  const handleOpen = (): void => {
    onClick(true);
  };
  const handleClose = (): void => {
    onClick(false);
  };

  return isOpen ? (
    <KeyboardArrowDown onClick={handleClose} />
  ) : (
    <KeyboardArrowRight onClick={handleOpen} />
  );
}
