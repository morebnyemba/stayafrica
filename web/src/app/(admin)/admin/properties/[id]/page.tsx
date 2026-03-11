'use client';

import { useState, useEffect } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { adminApi } from '@/lib/admin-api';
import { Property } from '@/types';
import toast from 'react-hot-toast';
import { Save, ArrowLeft, Building, MapPin, DollarSign, Users, CheckCircle } from 'lucide-react';
import Link from 'next/link';

export default function PropertyDetails() {
    const { id } = useParams() as { id: string };
    const router = useRouter();
    const [property, setProperty] = useState<Property | null>(null);
    const [loading, setLoading] = useState(true);
    const [saving, setSaving] = useState(false);

    // Form State
    const [title, setTitle] = useState('');
    const [description, setDescription] = useState('');
    const [pricePerNight, setPricePerNight] = useState(0);
    const [propertyType, setPropertyType] = useState('');
    const [bedrooms, setBedrooms] = useState(0);
    const [bathrooms, setBathrooms] = useState(0);
    const [maxGuests, setMaxGuests] = useState(0);
    const [status, setStatus] = useState<'active' | 'inactive' | 'pending_approval'>('pending_approval');

    // Location
    const [address, setAddress] = useState('');
    const [city, setCity] = useState('');
    const [country, setCountry] = useState('');

    useEffect(() => {
        loadProperty();
    }, [id]);

    const loadProperty = async () => {
        try {
            setLoading(true);
            const data = await adminApi.getPropertyById(id);
            setProperty(data);

            setTitle(data.title || '');
            setDescription(data.description || '');
            setPricePerNight(data.price_per_night || 0);
            setPropertyType(data.property_type || 'other');
            setBedrooms(data.bedrooms || 0);
            setBathrooms(data.bathrooms || 0);
            setMaxGuests(data.max_guests || 0);
            setStatus(data.status || 'pending_approval');

            if (data.location) {
                setAddress(data.location.address || '');
                setCity(data.location.city || '');
                setCountry(data.location.country || '');
            }
        } catch (err: any) {
            toast.error('Failed to load property details');
            console.error(err);
        } finally {
            setLoading(false);
        }
    };

    const handleSave = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!property) return;

        try {
            setSaving(true);

            const payload: Partial<Property> = {
                title,
                description,
                price_per_night: pricePerNight,
                property_type: propertyType as any,
                bedrooms,
                bathrooms,
                max_guests: maxGuests,
                status,
                location: {
                    ...property.location,
                    address,
                    city,
                    country,
                }
            };

            await adminApi.updateProperty(id, payload);
            toast.success('Property updated successfully');
            router.push('/admin/properties');
        } catch (err: any) {
            toast.error(err?.response?.data?.detail || 'Failed to update property');
            console.error(err);
        } finally {
            setSaving(false);
        }
    };

    if (loading) {
        return (
            <div className="flex justify-center items-center h-full min-h-[50vh]">
                <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-[#D9B168]"></div>
            </div>
        );
    }

    if (!property) {
        return (
            <div className="p-8 text-center">
                <h2 className="text-2xl font-bold text-red-600">Property Not Found</h2>
                <Link href="/admin/properties" className="text-[#3A5C50] hover:underline mt-4 inline-block">
                    Return to Properties List
                </Link>
            </div>
        );
    }

    return (
        <div className="p-8 max-w-5xl mx-auto">
            <div className="mb-8 flex items-center justify-between">
                <div className="flex items-center gap-4">
                    <Link href="/admin/properties" className="p-2 bg-sand-100 rounded-full hover:bg-sand-200 transition">
                        <ArrowLeft className="w-5 h-5 text-[#122F26]" />
                    </Link>
                    <div>
                        <h1 className="text-3xl font-bold text-[#122F26]">Edit Property</h1>
                        <p className="text-[#3A5C50] mt-1">{property.id}</p>
                    </div>
                </div>
            </div>

            <form onSubmit={handleSave} className="bg-white rounded-lg shadow-sm border border-sand-200 overflow-hidden">
                <div className="p-6 md:p-8 space-y-8">

                    {/* Status Actions header */}
                    <div className="flex items-center justify-between border-b pb-4">
                        <h3 className="text-lg font-bold text-[#122F26] flex items-center gap-2">
                            <CheckCircle className="w-5 h-5 text-green-600" /> Current Status:
                            <span className={`capitalize px-3 py-1 text-sm rounded-full ${status === 'active' ? 'bg-green-100 text-green-800' :
                                    status === 'inactive' ? 'bg-red-100 text-red-800' :
                                        'bg-yellow-100 text-yellow-800'
                                }`}>
                                {status.replace('_', ' ')}
                            </span>
                        </h3>

                        <select
                            value={status}
                            onChange={(e) => setStatus(e.target.value as any)}
                            className="px-4 py-2 border border-sand-300 rounded focus:ring-[#D9B168]"
                        >
                            <option value="active">Active (Published)</option>
                            <option value="inactive">Inactive (Suspended)</option>
                            <option value="pending_approval">Pending Approval</option>
                        </select>
                    </div>

                    <div className="grid grid-cols-1 md:grid-cols-2 gap-8">

                        {/* Basic Info */}
                        <div className="space-y-6">
                            <h4 className="text-md font-bold border-b pb-2 flex items-center gap-2">
                                <Building className="w-4 h-4" /> Basic Information
                            </h4>

                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-2">Title</label>
                                <input
                                    type="text"
                                    required
                                    value={title}
                                    onChange={(e) => setTitle(e.target.value)}
                                    className="w-full p-2 border border-sand-300 rounded focus:ring-[#D9B168]"
                                />
                            </div>

                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-2">Description</label>
                                <textarea
                                    required
                                    rows={4}
                                    value={description}
                                    onChange={(e) => setDescription(e.target.value)}
                                    className="w-full p-2 border border-sand-300 rounded focus:ring-[#D9B168]"
                                />
                            </div>

                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-2">Property Type</label>
                                <select
                                    required
                                    value={propertyType}
                                    onChange={(e) => setPropertyType(e.target.value)}
                                    className="w-full p-2 border border-sand-300 rounded focus:ring-[#D9B168]"
                                >
                                    <option value="apartment">Apartment</option>
                                    <option value="house">House</option>
                                    <option value="villa">Villa</option>
                                    <option value="cottage">Cottage</option>
                                    <option value="cosy_rooms">Cosy Rooms</option>
                                    <option value="other">Other</option>
                                </select>
                            </div>
                        </div>

                        {/* Pricing & Capacity */}
                        <div className="space-y-6">
                            <h4 className="text-md font-bold border-b pb-2 flex items-center gap-2">
                                <DollarSign className="w-4 h-4" /> Pricing & Capacity
                            </h4>

                            <div>
                                <label className="block text-sm font-medium text-[#122F26] mb-2">Price Per Night (USD)</label>
                                <input
                                    type="number"
                                    step="0.01"
                                    required
                                    value={pricePerNight}
                                    onChange={(e) => setPricePerNight(parseFloat(e.target.value))}
                                    className="w-full p-2 border border-sand-300 rounded focus:ring-[#D9B168]"
                                />
                            </div>

                            <div className="grid grid-cols-3 gap-4">
                                <div>
                                    <label className="block text-sm font-medium text-[#122F26] mb-2">Bedrooms</label>
                                    <input
                                        type="number"
                                        required
                                        min="0"
                                        value={bedrooms}
                                        onChange={(e) => setBedrooms(parseInt(e.target.value))}
                                        className="w-full p-2 border border-sand-300 rounded focus:ring-[#D9B168]"
                                    />
                                </div>
                                <div>
                                    <label className="block text-sm font-medium text-[#122F26] mb-2">Bathrooms</label>
                                    <input
                                        type="number"
                                        required
                                        min="0"
                                        step="0.5"
                                        value={bathrooms}
                                        onChange={(e) => setBathrooms(parseFloat(e.target.value))}
                                        className="w-full p-2 border border-sand-300 rounded focus:ring-[#D9B168]"
                                    />
                                </div>
                                <div>
                                    <label className="block text-sm font-medium text-[#122F26] mb-2 justify-between flex">
                                        <span>Max Guests</span>
                                        <Users className="w-4 h-4 text-sand-500" />
                                    </label>
                                    <input
                                        type="number"
                                        required
                                        min="1"
                                        value={maxGuests}
                                        onChange={(e) => setMaxGuests(parseInt(e.target.value))}
                                        className="w-full p-2 border border-sand-300 rounded focus:ring-[#D9B168]"
                                    />
                                </div>
                            </div>
                        </div>

                        {/* Location */}
                        <div className="space-y-6 md:col-span-2">
                            <h4 className="text-md font-bold border-b pb-2 flex items-center gap-2">
                                <MapPin className="w-4 h-4" /> Location Information
                            </h4>

                            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                                <div className="md:col-span-2">
                                    <label className="block text-sm font-medium text-[#122F26] mb-2">Street Address</label>
                                    <input
                                        type="text"
                                        required
                                        value={address}
                                        onChange={(e) => setAddress(e.target.value)}
                                        className="w-full p-2 border border-sand-300 rounded focus:ring-[#D9B168]"
                                    />
                                </div>
                                <div>
                                    <label className="block text-sm font-medium text-[#122F26] mb-2">City</label>
                                    <input
                                        type="text"
                                        required
                                        value={city}
                                        onChange={(e) => setCity(e.target.value)}
                                        className="w-full p-2 border border-sand-300 rounded focus:ring-[#D9B168]"
                                    />
                                </div>
                                <div>
                                    <label className="block text-sm font-medium text-[#122F26] mb-2">Country</label>
                                    <input
                                        type="text"
                                        required
                                        value={country}
                                        onChange={(e) => setCountry(e.target.value)}
                                        className="w-full p-2 border border-sand-300 rounded focus:ring-[#D9B168]"
                                    />
                                </div>
                            </div>
                        </div>

                    </div>
                </div>

                {/* Form Footer */}
                <div className="bg-[#F4F1EA] px-8 py-5 border-t border-sand-200 flex justify-end gap-4">
                    <Link href="/admin/properties">
                        <button type="button" className="px-6 py-2 border border-[#3A5C50] text-[#122F26] rounded-lg hover:bg-white transition-colors">
                            Cancel
                        </button>
                    </Link>
                    <button
                        type="submit"
                        disabled={saving}
                        className="flex items-center gap-2 bg-[#D9B168] hover:bg-[#c9a158] text-white px-6 py-2 rounded-lg font-medium transition disabled:opacity-50"
                    >
                        <Save className="w-5 h-5" />
                        {saving ? 'Saving...' : 'Save Changes'}
                    </button>
                </div>
            </form>
        </div>
    );
}
