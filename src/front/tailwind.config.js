/** @type {import('tailwindcss').Config} */
module.exports = {
    content: ["./*.sml", "../back/*.sml", "../dist/*.sml"],
    plugins: [require("@tailwindcss/typography"), require("daisyui")],
    theme: {
        container: {
            // you can configure the container to be centered
            center: true,
            // or have default horizontal padding
            padding: '1rem',

            screens: {
                sm: '640px',
                md: '768px',
                lg: '1024px',
                xl: '1024px',
                '2xl': '1024px',
            },
        },
    },
}

