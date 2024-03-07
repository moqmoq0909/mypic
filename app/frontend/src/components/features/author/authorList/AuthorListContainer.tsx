import AuthorList from "@features/author/authorList/AuthorList";
import useGetTable from "@features/author/authorList/hooks/useGetTable";
import createDataGrid from "@features/author/authorList/hooks/createDataGrid";

export default function AuthorListContainer() {
  // サーバーからデータを取得
  const { data, isLoading } = useGetTable(5);
  if (!data || isLoading) return <> </>;
  // AccordionTable用のデータに整形
  const gridData = createDataGrid(data);
  return <AuthorList gridData={gridData} />;
}
