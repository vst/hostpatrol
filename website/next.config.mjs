/** @type {import('next').NextConfig} */
const nextConfig = {
  basePath: '/lhp',
  output: 'export',
  images: {
    unoptimized: true,
    remotePatterns: [
      {
        protocol: 'https',
        hostname: 'cdn.simpleicons.org',
        port: '',
        pathname: '/**',
      },
    ],
  },
};

export default nextConfig;
