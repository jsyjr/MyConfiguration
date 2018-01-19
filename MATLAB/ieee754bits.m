function ieee754bits(varargin)
% IEEE754BITS   Print bits in floating-point numbers.
%
% Usage:
%    IEEE754BITS(X1, X1, ...) Displays the constituent bits of single and
%    double IEEE floating-point arguments, along with an indication of the 
%    bits' meaning.
%
%    IEEE754BITS, with no input arguments, returns the results for some
%    "interesting" numbers.
%
%    For "regular" (normal and denormalized) nonzero numbers, an equation
%    is given that describes the value according to the bits.  For other
%    numbers (e.g. Inf, NaN, 0), requisite conditions on the constituent
%    bits are shown.
%
%    For example:
%        ieee754bits(single(pi));
%
%    Displays:
%       sign exponent         mantissa
%          0 10000000 10010010000111111011011
%          | \______/ \_____________________/
%          |     |            ______|______________
%          |     |           /                     \
%     (-1)^0 2^(128 - 127) 1.10010010000111111011011 = 3.14159
%
%    The bits are displayed at the top, while the equation or condition
%    describing the number is located at the bottom.  Note that two number
%    systems are used in writing this equation: decimal notation is used
%    for the exponent, while binary is used for the mantissa.

%tstoolboxusage(mfilename('fullpath'),nargin,nargout); % Records usage of this function

% String inputs together into one vector
if nargin == 0
    x = [Inf ...                    % positive infinity
         realmax ...                % largest finite number
         2^53 - 1 ...               % last number where eps(x) == 1
         pi ...                     % 3.14159...
         1 ...                      % one
         realmin ...                % smallest positive non-denormalized number
         realmin - eps(realmin) ... % largest positive denormalized number (only because eps is constant under realmin)
         2^(1 - 1023 - 52) ...      % smallest positive number
         0 ...                      % zeros
         -0  ...                    % negative zero (-0 == 0 returns true)
         -1 ...                     % negative one
         -pi ...                    % negative pi
         -Inf ...                   % negative infinity
         NaN];                      % not-a-number
     
    dtype = class(x);
    N = length(x);
else
    N = 0;
    dtype = class(varargin{1});
    for n = 1:nargin
        if ~strcmp(class(varargin{n}), dtype)
            error('All inputs must be the same type');
        end
        N = N + numel(varargin{n});
    end
    x = zeros(N, 1, dtype);
    k = 0;
    for n = 1:nargin
        dk = numel(varargin{n});
        x(k + (1:dk)) = varargin{n}(:);
        k = k + dk;
    end
end

% Parameters for the types
switch dtype
    case 'single'
        sind = 1;         % sign
        einds = 2:9;      % exponent indices
        minds = 10:32;    % mantissa indices
        ebias = 127;      % exponent bias
        
        % sprintf strings for zero, denormal, Inf, NaN, and "regular"
        Tstr = '  sign exponent         mantissa';
        Bstr = '     %c %s %s';
        Cstr1 = '     | \______/ \_____________________/';
        Cstr2 = '     |     |            ______|______________';
        Cstr3 = '     |     |           /                     \';
        Zstr = '     %c   zeros                zeros            = %c0';
        Dstr = '(-1)^%c 2^(%3d - %d) 0.%s = %g (note: denormalized)';
        Istr = '     %c    ones                zeros            = %cInf';
        Nstr = '   any    ones            not all zeros        = NaN';
        Rstr = '(-1)^%c 2^(%3d - %d) 1.%s = %g';
        
    case 'double'
        sind = 1;
        einds = 2:12;
        minds = 13:64;
        ebias = 1023;
        
        Tstr  = '  sign   exponent                       mantissa';
        Bstr  = '     %c %s %s';
        Cstr1 = '     | \_________/ \__________________________________________________/';
        Cstr2 = '     |      |             ______________________|___________________________';
        Cstr3 = '     |      |            /                                                  \';
        Zstr  = '     %c    zeros                               zeros                           = %c0';
        Dstr  = '(-1)^%c 2^(%4d - %d) 0.%s = %g (note: denormalized)';
        Istr  = '     %c     ones                               zeros                           = %cInf';
        Nstr  = '   any     ones                           not all zeros                       = NaN';
        Rstr  = '(-1)^%c 2^(%4d - %d) 1.%s = %g';

    otherwise
        error('Input must be of type "double" or "single"');
end

% Generate the strings
for n = N:-1:1
    bits = local2bin(x(n));
    
    sbit  = bits(sind);
    ebits = bits(einds);
    mbits = bits(minds);
    
    snum = bin2dec(sbit);
    enum = bin2dec(ebits);
    mnum = bin2dec(mbits);

    if snum > 0
        sstr = '-';
    else
        sstr = '+';
    end
    
    A{n} = sprintf(Bstr, sbit, ebits, mbits);
    
    Zflag = all(ebits == '0') &&  all(mbits == '0'); % zero
    Dflag = all(ebits == '0') && ~all(mbits == '0'); % denormalized
    Iflag = all(ebits == '1') &&  all(mbits == '0'); % Inf
    Nflag = all(ebits == '1') && ~all(mbits == '0'); % NaN
    
    if     Zflag
        B{n} = sprintf(Zstr, sbit, sstr);
    elseif Dflag
        B{n} = sprintf(Dstr, sbit, enum, ebias - 1, mbits, x(n));
    elseif Iflag
        B{n} = sprintf(Istr, sbit, sstr);
    elseif Nflag
        B{n} = Nstr;
    else
        B{n} = sprintf(Rstr, sbit, enum, ebias, mbits, x(n));
    end
end

% Display them
disp(Tstr);
for n = 1:N;
    disp(A{n});
end
disp(Cstr1);
disp(Cstr2);
disp(Cstr3);
for n = 1:N
    disp(B{n});
end


function c = local2bin(x)

% the cast
y = typecast(x, 'uint8');

% switch bytes if needed
[str, maxsize, endian] = computer;
if endian == 'L'
    y = fliplr(y);
end

% convert to character
c = dec2bin(y, 8);
c = c';             % these lines...
c = c(:);           % ...scan c...
c = c';             % ...row-wise
