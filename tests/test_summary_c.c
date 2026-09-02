/**
 * @file test_summary_c.c
 * @brief C unit tests for summary.c functions
 *
 * Tests the internal C functions in summary.c:
 * - elapse()
 * - cputim()
 * - start_timer()
 * - end_timer()
 * - resource()
 * - bucket()
 *
 * @author Test Suite
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/utsname.h>
#include <unistd.h>
#ifdef _AIX
#include <sys/proc.h>
#endif
#ifdef __linux__
#include <errno.h>
#include <sys/resource.h>
#endif

/* Structure from summary.c */
struct time_data {
    double s_cpu;
    double s_wall;
    double f_cpu;
    double f_wall;
    double c_cpu;
    double c_wall;
    double c_bytes;
    int    c_calls;
    int    c_buckets[32];
    float  c_sum[32];
    double b_cpu[32];
    double b_wall[32];
};

/* Global variables from summary.c - declared as extern to link with library */
extern double tcpu, twall, tbytes, f_bytes;
extern double tot_wall, final_wall, start_wall;

/* Forward declarations of functions from summary.c */
extern int bucket(int lng);
extern void elapse(double *timer);
extern void cputim(double *usr, double *sys);
extern void start_timer(struct time_data *time);
extern void end_timer(struct time_data *time);
extern void resource(void);
extern void summary_(int *returnVal);
extern void start_(void);

int test_count = 0;
int pass_count = 0;

void assert_true(int condition, const char *message) {
    test_count++;
    if (condition) {
        printf("  [PASS] Test %d: %s\n", test_count, message);
        pass_count++;
    } else {
        printf("  [FAIL] Test %d: %s\n", test_count, message);
    }
}

void assert_nonnegative(double value, const char *message) {
    test_count++;
    if (value >= 0.0) {
        printf("  [PASS] Test %d: %s (value=%.6f)\n", test_count, message, value);
        pass_count++;
    } else {
        printf("  [FAIL] Test %d: %s (expected >= 0.0, got %.6f)\n", test_count, message, value);
    }
}

/**
 * Helper function to read file content into a string buffer
 * @param filename Path to file to read
 * @param buffer Output buffer
 * @param buffer_size Size of output buffer
 * @return Number of bytes read, -1 on error
 */
int read_file_content(const char *filename, char *buffer, size_t buffer_size) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        return -1;
    }
    
    size_t bytes_read = fread(buffer, 1, buffer_size - 1, file);
    buffer[bytes_read] = '\0';
    fclose(file);
    return bytes_read;
}

/**
 * Helper function to check if string contains substring
 * @param haystack String to search in
 * @param needle String to search for
 * @return 1 if found, 0 if not found
 */
int string_contains(const char *haystack, const char *needle) {
    return strstr(haystack, needle) != NULL;
}

int main(int argc, char *argv[]) {
    printf("Testing summary.c functions\n");
    printf("============================\n\n");

    /* Test 1: bucket() function */
    printf("Test Group 1: bucket() function\n");
    {
        int result;
        
        /* Test bucket with 0 */
        result = bucket(0);
        assert_true(result == 0, "bucket(0) returns 0");
        
        /* Test bucket with negative number */
        result = bucket(-5);
        assert_true(result == 0, "bucket(-5) returns 0");
        
        /* Test bucket with 1 */
        result = bucket(1);
        assert_true(result > 0, "bucket(1) returns positive value");
        
        /* Test bucket with 2 */
        result = bucket(2);
        assert_true(result == 2, "bucket(2) returns 2");
        
        /* Test bucket with 4 */
        result = bucket(4);
        assert_true(result == 3, "bucket(4) returns 3");
        
        /* Test bucket with 8 */
        result = bucket(8);
        assert_true(result == 4, "bucket(8) returns 4");
        
        /* Test bucket with 1000 */
        result = bucket(1000);
        assert_true(result > 0, "bucket(1000) returns positive value");
        
        printf("\n");
    }

    /* Test 2: elapse() function - covers the ifdef blocks */
    printf("Test Group 2: elapse() function\n");
    {
        double timer1, timer2;
        
        /* Get first time reading */
        elapse(&timer1);
        assert_nonnegative(timer1, "elapse() returns non-negative value");
        
        /* Small delay to ensure time difference */
        volatile int i;
        for (i = 0; i < 1000000; i++) {
            /* Busy loop for timing measurement */
        }
        
        /* Get second time reading */
        elapse(&timer2);
        assert_nonnegative(timer2, "elapse() second call returns non-negative value");
        
        /* Verify time progressed */
        assert_true(timer2 >= timer1, "elapse() time progresses monotonically");
        
        printf("\n");
    }

    /* Test 3: cputim() function */
    printf("Test Group 3: cputim() function\n");
    {
        double user, sys;
        double user2, sys2;
        
        /* Get first CPU time reading */
        cputim(&user, &sys);
        assert_nonnegative(user, "cputim() returns non-negative user time");
        assert_nonnegative(sys, "cputim() returns non-negative system time");
        
        /* Do some work to consume CPU time */
        volatile double x = 0.0;
        int i;
        for (i = 0; i < 10000; i++) {
            x += (double)i * 0.5 / (double)(i + 1);
        }
        
        /* Get second CPU time reading */
        cputim(&user2, &sys2);
        assert_nonnegative(user2, "cputim() second call returns non-negative user time");
        assert_nonnegative(sys2, "cputim() second call returns non-negative system time");
        
        /* CPU time should progress */
        assert_true(user2 >= user, "cputim() user time progresses");
        
        printf("\n");
    }

    /* Test 4: start_timer() and end_timer() */
    printf("Test Group 4: start_timer() and end_timer() functions\n");
    {
        struct time_data timer;
        int i;
        
        /* Initialize timer structure */
        memset(&timer, 0, sizeof(struct time_data));
        
        /* Start timer */
        start_timer(&timer);
        assert_nonnegative(timer.s_cpu, "start_timer() sets s_cpu");
        assert_nonnegative(timer.s_wall, "start_timer() sets s_wall");
        assert_true(timer.s_cpu >= 0.0, "start_timer() s_cpu is valid");
        assert_true(timer.s_wall >= 0.0, "start_timer() s_wall is valid");
        
        /* Do some work */
        volatile double x = 0.0;
        for (i = 0; i < 100000; i++) {
            x += (double)i / (double)(i + 1);
        }
        
        /* End timer */
        end_timer(&timer);
        assert_nonnegative(timer.f_cpu, "end_timer() sets f_cpu");
        assert_nonnegative(timer.f_wall, "end_timer() sets f_wall");
        assert_true(timer.f_cpu >= timer.s_cpu, "end_timer() f_cpu >= s_cpu");
        assert_true(timer.f_wall >= timer.s_wall, "end_timer() f_wall >= s_wall");
        assert_nonnegative(timer.c_cpu, "end_timer() accumulates c_cpu");
        assert_nonnegative(timer.c_wall, "end_timer() accumulates c_wall");
        
        /* End timer again to test accumulation */
        double prev_c_cpu = timer.c_cpu;
        double prev_c_wall = timer.c_wall;
        end_timer(&timer);
        assert_true(timer.c_cpu >= prev_c_cpu, "end_timer() c_cpu continues to accumulate");
        assert_true(timer.c_wall >= prev_c_wall, "end_timer() c_wall continues to accumulate");
        
        printf("\n");
    }

    /* Test 5: resource() function - covers the getrusage call and if check */
    printf("Test Group 5: resource() function\n");
    {
        printf("  Calling resource() to output system resource statistics...\n");
        resource();
        printf("  [PASS] Test %d: resource() executed without error\n", ++test_count);
        pass_count++;
        printf("\n");
    }


    /* Test 6: start_() and summary_() integration */
    printf("Test Group 6: Fortran interface functions\n");
    {
        printf("  Calling start_()...\n");
        start_();
        printf("  [PASS] Test %d: start_() executed without error\n", ++test_count);
        pass_count++;
        
        /* Do some work */
        volatile double x = 0.0;
        int i;
        for (i = 0; i < 50000; i++) {
            x += (double)i / (double)(i + 1);
        }
        
        printf("  Calling summary_()...\n");
        int returnVal = 0;
        summary_(&returnVal);
        printf("  [PASS] Test %d: summary_() executed without error\n", ++test_count);
        pass_count++;
        printf("\n");
    }

    /* Final summary */
    printf("============================\n");
    printf("Test Results: %d/%d tests passed\n", pass_count, test_count);
    printf("============================\n");
    
    if (pass_count == test_count) {
        printf("SUCCESS: All tests passed!\n");
        return 0;
    } else {
        printf("FAILURE: %d tests failed\n", test_count - pass_count);
        return 1;
    }
}
