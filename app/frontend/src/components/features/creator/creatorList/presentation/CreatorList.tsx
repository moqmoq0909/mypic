import { Flex, Button, Box } from "@radix-ui/themes";

import GlobalNavigation from "@features/generic/navigation/GlobalNavigation";
import CreatorBookRecord from "./CreatorBookRecord";

function CreatorList() {
  return (
    <GlobalNavigation>
      <Flex justify={"center"}>
        <Flex direction={"column"}>
          <CreatorBookRecord />
          <CreatorBookRecord />
          <CreatorBookRecord />
          <CreatorBookRecord />
          <CreatorBookRecord />
          <CreatorBookRecord />
          <CreatorBookRecord />
          <CreatorBookRecord />
          <CreatorBookRecord />
        </Flex>
      </Flex>
    </GlobalNavigation>
  );
}
export default CreatorList;
