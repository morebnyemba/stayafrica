import type { Metadata } from 'next';

export const metadata: Metadata = {
    title: 'Terms and Conditions',
    description: 'Read the terms and conditions for using StayAfrica.',
};

export default function TermsPage() {
    return (
        <div className="max-w-4xl mx-auto px-4 py-12 md:py-20">
            <h1 className="text-3xl md:text-5xl font-black text-primary-900 dark:text-sand-100 mb-6 tracking-tight">
                Terms and Conditions
            </h1>
            <p className="text-sm text-primary-500 dark:text-sand-400 mb-10">Last updated: {new Date().toLocaleDateString('en-US', { month: 'long', year: 'numeric' })}</p>

            <div className="prose prose-sand dark:prose-invert max-w-none">
                <section className="mb-8">
                    <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-100 mb-4">1. Introduction</h2>
                    <p className="text-primary-700 dark:text-sand-300 leading-relaxed mb-4">
                        Welcome to StayAfrica. These Terms and Conditions govern your use of our website and mobile application (collectively, the &quot;Platform&quot;). By accessing or using our Platform, you agree to be bound by these Terms and Conditions and our Privacy Policy.
                    </p>
                </section>

                <section className="mb-8">
                    <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-100 mb-4">2. Account Registration</h2>
                    <p className="text-primary-700 dark:text-sand-300 leading-relaxed mb-4">
                        To use certain features of our Platform, such as booking a property or listing one, you must register for an account. You agree to provide accurate, current, and complete information during the registration process and to update such information to keep it accurate, current, and complete.
                    </p>
                </section>

                <section className="mb-8">
                    <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-100 mb-4">3. Booking and Payments</h2>
                    <p className="text-primary-700 dark:text-sand-300 leading-relaxed mb-4">
                        When you book a property, you agree to pay all charges associated with the booking, including the nightly rate, applicable taxes, cleaning fees, and StayAfrica service fees. We partner with secure third-party payment gateways depending on your region (e.g., Paynow, PayFast, Stripe).
                    </p>
                </section>

                <section className="mb-8">
                    <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-100 mb-4">4. Host Obligations</h2>
                    <p className="text-primary-700 dark:text-sand-300 leading-relaxed mb-4">
                        If you choose to list your property on StayAfrica, you are responsible for ensuring that your listing is accurate, your property meets our quality standards, and you comply with all local laws and regulations governing short-term rentals.
                    </p>
                </section>

                <section className="mb-8">
                    <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-100 mb-4">5. Cancellations and Refunds</h2>
                    <p className="text-primary-700 dark:text-sand-300 leading-relaxed mb-4">
                        Cancellation policies vary by property and are set by the individual host. Please review the cancellation policy on the property listing before completing your booking. Refunds are subject to the specific property policy and our dispute resolution process.
                    </p>
                </section>

                <section className="mb-8">
                    <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-100 mb-4">6. Limitation of Liability</h2>
                    <p className="text-primary-700 dark:text-sand-300 leading-relaxed mb-4">
                        StayAfrica acts solely as an intermediary between hosts and guests. We are not responsible for the condition, legality, or suitability of any property listed on our Platform. To the maximum extent permitted by law, we exclude all liability for any damages arising out of your use of the Platform or your stay at a listed property.
                    </p>
                </section>

                <section className="mb-8">
                    <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-100 mb-4">7. Contact Us</h2>
                    <p className="text-primary-700 dark:text-sand-300 leading-relaxed mb-4">
                        If you have any questions about these Terms, please contact us at <a href="mailto:legal@stayafrica.app" className="text-accent-600 dark:text-accent-400 hover:underline">legal@stayafrica.app</a>.
                    </p>
                </section>
            </div>
        </div>
    );
}
