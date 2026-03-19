import { HeroSection } from '@/components/common/hero-section';
import { HomeProperties } from '@/components/property/home-properties';
import { HomeRedirect } from '@/components/common/home-redirect';

export default function Home() {
  return (
    <>
      <HomeRedirect />
      <HeroSection />
      <HomeProperties />
    </>
  );
}
