// コンポジットパターンなので作者も作品も同じカラムを持っている
import { useQuery } from "@tanstack/react-query";

export type Row = {
  id: string;
  name: string;
  uploadDate?: Date;
};
type BookTupple = [string, Row[]];

type ImageRecord = {
  imageCode: string;
  imageName: string;
  imageUrl: string;
  bookId: number;
  uploadedAt: Date;
};

type TagRecord = {
  tagCode: string;
  tagName: string;
  creatorId: number;
};

type CategoryRecord = {
  categoryCode: string;
  categoryName: string;
  categoryColor: string;
};

// 全ての要素を<TagGroup>として描画するので、Mapには入れずmapする
export type CategoryTupple = [CategoryRecord, TagRecord[]];

// Mapに入れて、book_codeでgetする。
// creator_code, image_codeでもgetできるよう、特定のkey名を付けずstringにしてある
type TagTupple = [string, CategoryTupple[]];

export type TableData = {
  creators: Row[];
  books: BookTupple[];
  images: ImageRecord[];
  tags: TagTupple[];
};

// const fetchTable = async (): Promise<TableData> => {
//   const res = await fetch('http://localhost:8081/table')
//   return res.json()
// }

const searchByTagId = async (tagId: number) => {
  const res = await fetch("http://localhost:8081/table", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify(tagId),
  });

  if (!res.ok) {
    throw new Error("Network response was not ok");
  }

  return res.json();
};

const useGetTable = (tagId: number) =>
  useQuery<TableData>({
    queryKey: ["table"],
    queryFn: () => searchByTagId(tagId),
  });

export default useGetTable;
