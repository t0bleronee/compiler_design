#ifndef ERRORS_H
#define ERRORS_H

#include <string>
#include <vector>
#include <iostream>

struct ErrorInfo {
    int lineNumber;
    std::string message;
    
    ErrorInfo(int line, const std::string& msg) : lineNumber(line), message(msg) {}
};

class ErrorHandler {
private:
    std::vector<ErrorInfo> errors;
    int errorCount;
    int warningCount;

public:
    ErrorHandler() : errorCount(0), warningCount(0) {}
    
    // Add an error
    void addError(int lineNumber, const std::string& message) {
        errors.push_back(ErrorInfo(lineNumber, message));
        errorCount++;
    }
    
    // Add a warning
    void addWarning(int lineNumber, const std::string& message) {
        errors.push_back(ErrorInfo(lineNumber, "WARNING: " + message));
        warningCount++;
    }
    
    // Print all errors and warnings
    void printErrors() const {
        if (errors.empty()) {
            std::cout << "\nNo errors or warnings found." << std::endl;
            return;
        }
        
        std::cout << "\n=== ERROR REPORT ===" << std::endl;
        for (const ErrorInfo& error : errors) {
            std::cout << "Line " << error.lineNumber << ": " << error.message << std::endl;
        }
        
        std::cout << "\nTotal Errors: " << errorCount << std::endl;
        std::cout << "Total Warnings: " << warningCount << std::endl;
    }
    
    // Check if there are any errors
    bool hasErrors() const {
        return errorCount > 0;
    }
    
    // Check if there are any warnings
    bool hasWarnings() const {
        return warningCount > 0;
    }
    
    // Get error count
    int getErrorCount() const {
        return errorCount;
    }
    
    // Get warning count
    int getWarningCount() const {
        return warningCount;
    }
    
    // Clear all errors and warnings
    void clear() {
        errors.clear();
        errorCount = 0;
        warningCount = 0;
    }
    
    // Get all errors
    const std::vector<ErrorInfo>& getErrors() const {
        return errors;
    }
    
    // Print summary
    void printSummary() const {
        if (errorCount == 0 && warningCount == 0) {
            std::cout << "\n✓ Compilation successful - No errors or warnings" << std::endl;
        } else if (errorCount == 0) {
            std::cout << "\n⚠ Compilation successful with " << warningCount << " warning(s)" << std::endl;
        } else {
            std::cout << "\n✗ Compilation failed with " << errorCount << " error(s)";
            if (warningCount > 0) {
                std::cout << " and " << warningCount << " warning(s)";
            }
            std::cout << std::endl;
        }
    }
};

// Global error handler instance
extern ErrorHandler errors;

#endif // ERRORS_H
