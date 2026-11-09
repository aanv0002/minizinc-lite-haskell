import pluginChecker from "vite-plugin-checker";
import { defineConfig } from "vite";

export default defineConfig({
    plugins: [pluginChecker({ typescript: true, overlay: false })],
    server: {
        host: "127.0.0.1",
        port: 8080,
        proxy: {
            "/api": {
                target: "http://127.0.0.1:3000",
                changeOrigin: true,
            },
        },
    },
});
