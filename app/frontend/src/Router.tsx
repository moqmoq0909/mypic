import { BrowserRouter, Routes, Route } from "react-router-dom";

// import Home from "@features/home/Home";
import Gallery from "@ui/gallery/Gallery";
import CreatorListContainer from "@features/creator/creatorList/application/CreatorListContainer";
import AuthorListContainer from "@features/author/authorList/AuthorListContainer";

function RouterConfig() {
  return (
    <Routes>
      {/* <Route path={"/"} element={<Home />} /> */}
      <Route path={"/"} element={<CreatorListContainer />} />
      <Route path={"/gallery"} element={<Gallery />} />
      <Route path={"/table"} element={<AuthorListContainer />} />
    </Routes>
  );
}

function Router() {
  return (
    <BrowserRouter>
      <Routes>
        <Route path={"/*"} element={<RouterConfig />} />
      </Routes>
    </BrowserRouter>
  );
}
export default Router;
