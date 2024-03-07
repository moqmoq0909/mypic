import { ReactNode } from "react";

type NavigationProps = {
  pageTitle: string;
  children: ReactNode;
  /* eslint react/require-default-props: 0 */
  Header?: ReactNode;
  Footer?: ReactNode;
};

function Navigation({
  pageTitle,
  children,
  Header = <div>デフォルトヘッダー</div>,
  Footer = <div>デフォルトフッター</div>,
}: NavigationProps) {
  const t = pageTitle;
  return (
    <>
      {Header}
      {children}
      {Footer}
    </>
  );
}

export default function Home() {
  return (
    <Navigation
      pageTitle={"ホーム"}
      Header={<div>ヘッダー</div>}
      // Footer={<div>フッダー</div>}
    >
      <div>メイン</div>
    </Navigation>
  );
}
