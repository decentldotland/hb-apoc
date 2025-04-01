import React from 'react';

/**
 * LoadingSpinner component for showing loading state
 * 
 * @param {string} size - Size of the spinner (sm, md, lg)
 * @param {string} color - Color of the spinner (green, accent, primary)
 */
const LoadingSpinner = ({ size = 'md', color = 'green' }) => {
  // Define size classes
  const sizeClasses = {
    sm: 'w-4 h-4',
    md: 'w-8 h-8',
    lg: 'w-12 h-12',
    xl: 'w-16 h-16'
  };
  
  // Define color classes
  const colorClasses = {
    green: 'border-terminal-green',
    accent: 'border-terminal-accent',
    primary: 'border-terminal-primary',
    white: 'border-white'
  };
  
  // Get the appropriate classes
  const spinnerSize = sizeClasses[size] || sizeClasses.md;
  const spinnerColor = colorClasses[color] || colorClasses.green;
  
  return (
    <div className="flex items-center justify-center">
      <div className={`${spinnerSize} border-2 border-t-transparent ${spinnerColor} rounded-full animate-spin`}></div>
      <span className="sr-only">Loading...</span>
    </div>
  );
};

export default LoadingSpinner;
