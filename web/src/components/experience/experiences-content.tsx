'use client';

import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { apiClient } from '@/services/api-client';
import { ExperienceCard } from './experience-card';
import { Button } from '@/components/ui';
import { Filter, Search } from 'lucide-react';

export function ExperiencesContent() {
  const [searchTerm, setSearchTerm] = useState('');
  const [selectedCategory, setSelectedCategory] = useState('');
  const [selectedDifficulty, setSelectedDifficulty] = useState('');
  const [showFilters, setShowFilters] = useState(false);

  // Fetch categories
  const { data: categoriesData } = useQuery({
    queryKey: ['experience-categories'],
    queryFn: async () => {
      const response = await apiClient.getExperienceCategories();
      return response.data;
    },
  });

  // Fetch experiences
  const {
    data: experiencesData,
    isLoading,
    error,
  } = useQuery({
    queryKey: ['experiences', searchTerm, selectedCategory, selectedDifficulty],
    queryFn: async () => {
      const params: any = {};
      if (searchTerm) params.search = searchTerm;
      if (selectedCategory) params.category = selectedCategory;
      if (selectedDifficulty) params.difficulty = selectedDifficulty;

      const response = await apiClient.getExperiences(params);
      return response.data;
    },
  });

  const experiences = experiencesData?.results || [];
  const categories = categoriesData || [];

  const handleSearch = (e: React.FormEvent) => {
    e.preventDefault();
    // Search is already reactive through the query key
  };

  return (
    <div className="bg-sand-100 dark:bg-primary-900 min-h-screen">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
        {/* Header */}
        <div className="mb-8">
          <h1 className="text-3xl md:text-4xl font-bold text-primary-900 dark:text-sand-50 mb-4">
            Experiences
          </h1>
          <p className="text-lg text-primary-600 dark:text-sand-200">
            Discover unique activities and adventures across Africa
          </p>
        </div>

        {/* Search Bar */}
        <div className="card p-4 sm:p-6 mb-6">
          <form onSubmit={handleSearch} className="flex flex-col sm:flex-row gap-3">
            <div className="flex-1 relative">
              <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-primary-400 w-5 h-5" />
              <input
                type="text"
                placeholder="Search experiences..."
                value={searchTerm}
                onChange={(e) => setSearchTerm(e.target.value)}
                className="w-full pl-10 pr-4 py-2 border border-primary-200 dark:border-primary-600 rounded-lg bg-sand-50 dark:bg-primary-700 text-primary-900 dark:text-sand-100 focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
              />
            </div>
            <Button
              type="button"
              onClick={() => setShowFilters(!showFilters)}
              variant="outline"
              className="sm:w-auto"
            >
              <Filter className="w-4 h-4 mr-2" />
              Filters
            </Button>
          </form>

          {/* Filters */}
          {showFilters && (
            <div className="mt-4 pt-4 border-t border-primary-200 dark:border-primary-700">
              <div className="grid grid-cols-1 sm:grid-cols-2 gap-4">
                {/* Category Filter */}
                <div>
                  <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
                    Category
                  </label>
                  <select
                    value={selectedCategory}
                    onChange={(e) => setSelectedCategory(e.target.value)}
                    className="w-full px-3 py-2 border border-primary-200 dark:border-primary-600 rounded-lg bg-sand-50 dark:bg-primary-700 text-primary-900 dark:text-sand-100 focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
                  >
                    <option value="">All Categories</option>
                    {categories.map((category: any) => (
                      <option key={category.id} value={category.id}>
                        {category.name}
                      </option>
                    ))}
                  </select>
                </div>

                {/* Difficulty Filter */}
                <div>
                  <label className="block text-sm font-medium text-primary-900 dark:text-sand-50 mb-2">
                    Difficulty
                  </label>
                  <select
                    value={selectedDifficulty}
                    onChange={(e) => setSelectedDifficulty(e.target.value)}
                    className="w-full px-3 py-2 border border-primary-200 dark:border-primary-600 rounded-lg bg-sand-50 dark:bg-primary-700 text-primary-900 dark:text-sand-100 focus:ring-2 focus:ring-secondary-500 focus:border-transparent"
                  >
                    <option value="">All Difficulties</option>
                    <option value="easy">Easy</option>
                    <option value="moderate">Moderate</option>
                    <option value="challenging">Challenging</option>
                    <option value="expert">Expert</option>
                  </select>
                </div>
              </div>

              {/* Clear Filters */}
              {(selectedCategory || selectedDifficulty) && (
                <div className="mt-4">
                  <Button
                    type="button"
                    onClick={() => {
                      setSelectedCategory('');
                      setSelectedDifficulty('');
                    }}
                    variant="outline"
                    size="sm"
                  >
                    Clear Filters
                  </Button>
                </div>
              )}
            </div>
          )}
        </div>

        {/* Experiences Grid */}
        {isLoading ? (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            {[1, 2, 3, 4, 5, 6].map((i) => (
              <div key={i} className="card p-6 animate-pulse">
                <div className="h-48 bg-primary-200 dark:bg-primary-700 rounded mb-4"></div>
                <div className="h-6 bg-primary-200 dark:bg-primary-700 rounded mb-2 w-3/4"></div>
                <div className="h-4 bg-primary-200 dark:bg-primary-700 rounded mb-2"></div>
                <div className="h-4 bg-primary-200 dark:bg-primary-700 rounded w-5/6"></div>
              </div>
            ))}
          </div>
        ) : error ? (
          <div className="card p-12 text-center">
            <p className="text-primary-600 dark:text-sand-300 mb-4">
              Unable to load experiences. Please try again later.
            </p>
            <p className="text-sm text-primary-500 dark:text-sand-400">
              Error: {error instanceof Error ? error.message : 'Unknown error'}
            </p>
          </div>
        ) : experiences.length === 0 ? (
          <div className="card p-12 text-center">
            <h3 className="text-xl font-semibold text-primary-900 dark:text-sand-50 mb-2">
              No Experiences Found
            </h3>
            <p className="text-primary-600 dark:text-sand-300 mb-4">
              {searchTerm || selectedCategory || selectedDifficulty
                ? 'Try adjusting your filters or search terms.'
                : 'Experiences will be available soon. Check back later!'}
            </p>
            {(searchTerm || selectedCategory || selectedDifficulty) && (
              <Button
                onClick={() => {
                  setSearchTerm('');
                  setSelectedCategory('');
                  setSelectedDifficulty('');
                }}
              >
                Clear Filters
              </Button>
            )}
          </div>
        ) : (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            {experiences.map((experience: any) => (
              <ExperienceCard key={experience.id} experience={experience} />
            ))}
          </div>
        )}

        {/* Results Count */}
        {!isLoading && experiences.length > 0 && (
          <div className="mt-8 text-center text-sm text-primary-600 dark:text-sand-400">
            Showing {experiences.length} {experiences.length === 1 ? 'experience' : 'experiences'}
          </div>
        )}
      </div>
    </div>
  );
}
