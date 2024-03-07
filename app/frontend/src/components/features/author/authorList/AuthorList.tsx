// import { AuthorOrWorkRecord } from '@/components/features/author/authorList/hooks/useGetAuthorList'
// import MultiPullDownMenu from "@ui/navigation/MultiPullDownMenu";
import { useState, useEffect } from "react";
import AccordionTable, { ATGridData } from "@ui/dataGrid/AccordionTable";
import GlobalNavigation from "@features/generic/navigation/GlobalNavigation";
import usePostTweetUpload from "@features/author/authorList/hooks/usePostTweetUpload";

import { Box, Button } from "@mui/material";
import { Progress } from "@mantine/core";

type Record = {
  id: string;
  name: string;
};

type AuthorListProps = {
  gridData: ATGridData<Record>;
};

const menus = [
  {
    menuLabel: "ギャラリー",
    menuItems: [
      { itemLabel: "ギャラリー", onClick: () => {} },
      { itemLabel: "ギャラリー", onClick: () => {} },
    ],
  },
  {
    menuLabel: "作品",
    menuItems: [
      { itemLabel: "作品", onClick: () => {} },
      { itemLabel: "作品", onClick: () => {} },
    ],
  },
];
export default function AuthorList({ gridData }: AuthorListProps) {
  const [twitterAccountName, setTwitterAccountName] = useState<string>("");
  const { mutate } = usePostTweetUpload();

  // WebSocket接続を開始する
  useEffect(() => {
    const websocket = new WebSocket("stream://localhost:8081");

    // WebSocketイベントハンドラ
    websocket.onopen = () => {
      console.log("WebSocket connected");
    };
    websocket.onmessage = (event) => {
      console.log("受け取ったメッセージ:", event.data);
    };
    websocket.onclose = () => {
      console.log("WebSocket disconnected");
    };

    // コンポーネントアンマウント時にWebSocket接続を閉じる
    return () => {
      websocket.close();
    };
  }, []);

  return (
    <GlobalNavigation>
      {/* <MultiPullDownMenu menus={menus} /> */}
      <Button onClick={() => mutate(twitterAccountName)}>ツイッターのいいねを同期</Button>
      <Progress radius={"xs"} size={"xl"} value={50} striped animated />
      <div>アコーディオン</div>
      <AccordionTable gridData={gridData} />
    </GlobalNavigation>
  );
}
