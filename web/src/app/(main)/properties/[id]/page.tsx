'use client';

import { useEffect, useState } from 'react';
import { PropertyDetailContent } from '@/components/property/property-detail';

export default function PropertyPage({ params }: { params: Promise<{ id: string }> }) {
    const [propertyId, setPropertyId] = useState<string | null>(null);

    useEffect(() => {
        async function initializeParams() {
            const resolvedParams = await params;
            setPropertyId(resolvedParams.id);
        }
        initializeParams();
    }, [params]);

    if (!propertyId) {
        return (
            <div className="max-w-7xl mx-auto px-4 py-12">
                <div className="text-center">
                    <p className="text-primary-900 dark:text-sand-100">Loading...</p>
                </div>
            </div>
        );
    }

    return <PropertyDetailContent propertyId={propertyId} />;
}
