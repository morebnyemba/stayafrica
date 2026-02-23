import type { Metadata } from 'next';

export const metadata: Metadata = {
    title: 'Cookie Policy',
    description: 'Learn how StayAfrica uses cookies to enhance your experience.',
};

export default function CookiesPage() {
    return (
        <div className="max-w-4xl mx-auto px-4 py-12 md:py-20">
            <h1 className="text-3xl md:text-5xl font-black text-primary-900 dark:text-sand-100 mb-6 tracking-tight">
                Cookie Policy
            </h1>
            <p className="text-sm text-primary-500 dark:text-sand-400 mb-10">Last updated: {new Date().toLocaleDateString('en-US', { month: 'long', year: 'numeric' })}</p>

            <div className="prose prose-sand dark:prose-invert max-w-none">
                <section className="mb-8">
                    <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-100 mb-4">1. What are Cookies?</h2>
                    <p className="text-primary-700 dark:text-sand-300 leading-relaxed mb-4">
                        Cookies are small text files that are placed on your computer or mobile device when you visit a website. They are widely used to make websites work more efficiently and to provide information to the owners of the site.
                    </p>
                </section>

                <section className="mb-8">
                    <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-100 mb-4">2. How We Use Cookies</h2>
                    <p className="text-primary-700 dark:text-sand-300 leading-relaxed mb-4">
                        StayAfrica uses cookies for several reasons:
                    </p>
                    <ul className="list-disc pl-5 text-primary-700 dark:text-sand-300 space-y-2 mb-4">
                        <li><strong>Essential Cookies:</strong> These are required for the operation of our platform, such as enabling you to log into secure areas of our website.</li>
                        <li><strong>Performance & Analytics Cookies:</strong> These allow us to recognize and count the number of visitors and to see how visitors move around our platform. This helps us to improve the way our website works.</li>
                        <li><strong>Functionality Cookies:</strong> These are used to recognize you when you return to our platform. This enables us to personalize our content for you and remember your preferences (e.g., language or region).</li>
                        <li><strong>Targeting Cookies:</strong> These record your visit to our platform, the pages you have visited, and the links you have followed. We use this information to make our platform more relevant to your interests.</li>
                    </ul>
                </section>

                <section className="mb-8">
                    <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-100 mb-4">3. Managing Cookies</h2>
                    <p className="text-primary-700 dark:text-sand-300 leading-relaxed mb-4">
                        You can set your browser to refuse all or some browser cookies, or to alert you when websites set or access cookies. If you disable or refuse cookies, please note that some parts of the StayAfrica platform may become inaccessible or not function properly.
                    </p>
                </section>

                <section className="mb-8">
                    <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-100 mb-4">4. Changes to This Policy</h2>
                    <p className="text-primary-700 dark:text-sand-300 leading-relaxed mb-4">
                        We may update our Cookie Policy from time to time. We will notify you of any changes by posting the new Cookie Policy on this page. You are advised to review this policy periodically for any changes.
                    </p>
                </section>

                <section className="mb-8">
                    <h2 className="text-2xl font-bold text-primary-900 dark:text-sand-100 mb-4">5. Contact Us</h2>
                    <p className="text-primary-700 dark:text-sand-300 leading-relaxed mb-4">
                        If you have any questions about our use of cookies, please contact us at <a href="mailto:privacy@stayafrica.app" className="text-accent-600 dark:text-accent-400 hover:underline">privacy@stayafrica.app</a>.
                    </p>
                </section>
            </div>
        </div>
    );
}
