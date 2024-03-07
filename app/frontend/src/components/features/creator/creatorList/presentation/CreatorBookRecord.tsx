import { Flex, Box, Separator, Avatar, Text, IconButton } from "@radix-ui/themes";
import { TwitterLogoIcon, InstagramLogoIcon } from "@radix-ui/react-icons";

import VerticalScroll from "@ui/navigation/verticalScroll/VerticalScroll";

function CreatorBookRecord() {
  return (
    <Flex direction={"column"}>
      <Flex gap={"3"} my={"3"}>
        <Avatar
          size={"7"}
          src={
            "https://images.unsplash.com/photo-1607346256330-dee7af15f7c5?&w=64&h=64&dpr=2&q=70&crop=focalpoint&fp-x=0.67&fp-y=0.5&fp-z=1.4&fit=crop"
          }
          radius={"full"}
          fallback={"T"}
        />
        <Flex direction={"column"} justify={"between"}>
          <Box style={{ width: "200px" }}>
            <Text as={"div"} size={"2"} weight={"bold"}>
              Teodros Girmay
            </Text>
            <Text as={"div"} size={"2"} color={"gray"}>
              Engineering
            </Text>
          </Box>
          <Box>
            {/* <IconButton size={"4"} variant={"ghost"}> */}
            <TwitterLogoIcon width={25} height={25} />
            {/* </IconButton> */}
            <InstagramLogoIcon width={25} height={25} />
          </Box>
        </Flex>
        <VerticalScroll />
      </Flex>
      <Separator style={{ width: "100%" }} />
    </Flex>
  );
}
export default CreatorBookRecord;
