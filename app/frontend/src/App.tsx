// import "./App.css";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";

import { Theme } from "@radix-ui/themes";
import { createTheme, MantineProvider } from "@mantine/core";
import Router from "./Router";
import "@radix-ui/themes/styles.css";
import "@mantine/core/styles.css";

const queryClient = new QueryClient();

function App() {
  const theme = createTheme({
    /** Your theme override here */
  });
  return (
    <QueryClientProvider client={queryClient}>
      <Theme>
        <MantineProvider theme={theme}>
          <Router />
        </MantineProvider>
      </Theme>
    </QueryClientProvider>
  );
}

export default App;
