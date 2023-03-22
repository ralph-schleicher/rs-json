%% bench.m --- measure performance of JSON libraries

% Copyright (C) 2023 Ralph Schleicher

% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions
% are met:
%
%    * Redistributions of source code must retain the above copyright
%      notice, this list of conditions and the following disclaimer.
%
%    * Redistributions in binary form must reproduce the above copyright
%      notice, this list of conditions and the following disclaimer in
%      the documentation and/or other materials provided with the
%      distribution.
%
%    * Neither the name of the copyright holder nor the names of its
%      contributors may be used to endorse or promote products derived
%      from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
% FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.

%% Code:

function bench(stem)

  if nargin < 1
    error('Too few arguments');
  end

  % Column labels.
  lisp = {'SBCL', ...
          'CCL'};

  % Row labels.
  lib = {'cl-json', ...
         'jonathan', ...
         'json-streams', ...
         'jsown', ...
         'jzon', ...
         'rs-json', ...
         'shasht', ...
         'st-json', ...
         'yason'};

  job = {'read', ...
         'write'};

  % Table data (run time).
  time = nan(numel(lib), numel(lisp), numel(job));
  % Table data (bytes consed).
  mem = nan(numel(lib), numel(lisp), numel(job));

  c = textscan(xfileread([stem, '.csv']), '%s%s%s%s%f\n', 'Delimiter', ';');
  [lisp1, lib1, job1, tag1, val1] = deal(c{:});

  for k = 1:numel(job)
    for j = 1:numel(lisp)
      for i = 1:numel(lib)
        k1 = strcmpi(lisp1, lisp{j}) & strcmpi(lib1, lib{i}) & strcmpi(job1, job{k});
        k2 = k1 & strcmpi(tag1, 'err');
        if ~ any(k2)
          k2 = k1 & strcmpi(tag1, 'run');
          time(i, j, k) = mean(val1(k2));
          k2 = k1 & strcmpi(tag1, 'mem');
          mem(i, j, k) = mean(val1(k2));
        end
      end
    end
  end

  switch stem
   case 'citm_catalog'
    time_lim = 0.3;
    mem_lim = 35;
   case 'large'
    time_lim = 30;
    mem_lim = 3500;
   otherwise
    error('Fix me.');
  end

  % Absolute numbers.
  time_read = squeeze(time(:, :, 1));
  time_write = squeeze(time(:, :, 2));
  mem_read = squeeze(mem(:, :, 1)) ./ 2^20;
  mem_write = squeeze(mem(:, :, 2)) ./ 2^20;

  [fig, ax] = figure1(2, 2);
  plot1(ax(1, 1), lisp, lib, time_read,  time_lim, 'Run Time / s', ['Read ', stem, '.json'],  'northwest');
  plot1(ax(1, 2), lisp, lib, time_write, time_lim, 'Run Time / s', ['Write ', stem, '.json'], 'northeast');
  plot1(ax(2, 1), lisp, lib, mem_read,   mem_lim,  'Bytes Consed / MiB', '', '');
  plot1(ax(2, 2), lisp, lib, mem_write,  mem_lim,  'Bytes Consed / MiB', '', '');
  print1(fig, [stem, '-absolute']);

  % Relative numbers.
  ref = find(strcmp(lib, 'rs-json'));
  time_read_rel = time_read ./ time_read(ref, :) .* 100;
  time_write_rel = time_write ./ time_write(ref, :) .* 100;
  mem_read_rel = mem_read ./ mem_read(ref, :) .* 100;
  mem_write_rel = mem_write ./ mem_write(ref, :) .* 100;

  [fig, ax] = figure1(2, 2);
  plot1(ax(1, 1), lisp, lib, time_read_rel,  350, 'Run Time / %', ['Read ', stem, '.json'],  'northwest');
  plot1(ax(1, 2), lisp, lib, time_write_rel, 350, 'Run Time / %', ['Write ', stem, '.json'], 'northeast');
  plot1(ax(2, 1), lisp, lib, mem_read_rel,   350, 'Bytes Consed / %', '', '');
  plot1(ax(2, 2), lisp, lib, mem_write_rel,  350, 'Bytes Consed / %', '', '');
  print1(fig, [stem, '-relative']);

function [fig, ax] = figure1(m, n)

  fig = figure;
  ax = axes_layout(fig, ...
                   'Rows', m, ...
                   'Columns', n, ...
                   'Grid', 'off', ...
                   'Width', 8, ...
                   'Height', 4, ...
                   'Margin', [0.75 0.25 0.5 0.5], ...
                   'Padding', [1 0.75]);

function print1(fig, name)

  fig_print(fig, '-A4', '-landscape', '-fit=on', [name, '.png']);

function plot1(ax, lisp, lib, data, lim, tag, name, loc)

  axes(ax);

  h = bar(1:numel(lib), xmin(data, lim), 0.7);
  set(h(1), 'FaceColor', color_blue);
  set(h(2), 'FaceColor', color_yellow);

  set(ax, 'Box', 'on', ...
          'Layer', 'bottom', ...
          'YGrid', 'on');

  xlim([0, numel(lib)] + 0.5);
  xticks(1:numel(lib));
  xticklabels(lib);
  len = get(ax, 'TickLength');
  len(1) = 0;
  set(ax, 'TickLength', len);

  ylim([0, lim]);
  ylabel(tag);

  if ~ isempty(name)
    title(name, 'Interpreter', 'none');
  end

  if ~ isempty(loc)
    legend(lisp, 'Location', loc);
  end

  p = 10^(floor(log10(lim)) - 1);
  for i = 1:numel(lib)
    for j = 1:numel(lisp)
      if j == 1
        x = i - 0.7/5;
      else
        x = i + 0.7/5;
      end
      if isnan(data(i, j))
        s = 'N/A';
        y = 0;
      elseif data(i, j) > lim
        s = sprintf('%G', xround(data(i, j), p));
        y = lim;
      else
        s = '';
      end
      if ~ isempty(s)
        text(x, y + p / 4, s, ...
             'HorizontalAlignment', 'center', ...
             'VerticalAlignment', 'bottom', ...
             'Interpreter', 'none', ...
             'FontSize', 7);
      end
    end
  end

function c = xmin(a, b)

  k = isnan(a) | isnan(b);
  c = min(a, b);
  c(k) = nan;

%% bench.m ends here
