import { HeroSection } from '@/components/common/hero-section';
import { FeaturedProperties } from '@/components/property/featured-properties';
import { SearchSection } from '@/components/common/search-section';
import { HowItWorks } from '@/components/common/how-it-works';
import { Testimonials } from '@/components/common/testimonials';

export default function Home() {
  return (
    <>
      <HeroSection />
      <SearchSection />
      <FeaturedProperties />
      <HowItWorks />
      <Testimonials />
    </>
  );
}
