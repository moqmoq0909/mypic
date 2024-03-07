import { useQuery } from "@tanstack/react-query";

const fetchUsers = async () => {
  const res = await fetch("http://localhost:8081/creators");
  return res.json();
};

type Creator = {
  creatorId: number;
  creatorName: string;
  creatorGroupId: number;
};

function HaskellApi() {
  const { data: creators, isLoading } = useQuery<Creator[]>({
    queryKey: ["creators"],
    queryFn: fetchUsers,
    initialData: [],
  });
  console.log("isLoadingです！！", isLoading);

  return (
    <>
      <h1>Todo一覧</h1>
      <ul>
        {creators?.map((creator) => (
          <li key={creator.creatorId}>
            id:{creator.creatorId},name:{creator.creatorName},groupId
            {creator.creatorGroupId}
          </li>
        ))}
      </ul>
    </>
  );
}

export default HaskellApi;
