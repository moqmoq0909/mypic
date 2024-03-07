import { defineConfig } from "vite";
import react from "@vitejs/plugin-react-swc";

// https://vitejs.dev/config/
export default defineConfig({
  resolve: {
    // Importパスにエイリアスを設定
    alias: [
      { find: "@", replacement: "/src" },
      { find: "@common", replacement: "/src/common" },
      { find: "@features", replacement: "/src/components/features" },
      { find: "@hooks", replacement: "/src/components/hooks" },
      { find: "@pages", replacement: "/src/components/pages" },
      { find: "@ui", replacement: "/src/components/ui" },
    ],
  },
  plugins: [react()],
});
