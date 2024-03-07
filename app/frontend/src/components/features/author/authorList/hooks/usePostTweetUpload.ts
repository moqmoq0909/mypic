import { useMutation } from "@tanstack/react-query";

export type TweetUploadRes = {
  status: number;
  message: string;
};

const tweetUploadByUserName = async (userName: string) => {
  const res = await fetch("http://localhost:8081/tweet-upload", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify(userName),
  });

  if (!res.ok) {
    throw new Error("Network response was not ok");
  }

  return res.json();
};

const usePostTweetUpload = () =>
  useMutation<TweetUploadRes, Error, string>({
    mutationFn: (twitterUserName: string) => tweetUploadByUserName(twitterUserName),
  });

export default usePostTweetUpload;
